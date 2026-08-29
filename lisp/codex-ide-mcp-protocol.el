;;; codex-ide-mcp-protocol.el --- MCP protocol dispatch for Codex  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Thanos Apollo

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;;; Commentary:

;; JSON-RPC and MCP method handling for the local MCP bridge.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'codex-ide-mcp-core)
(require 'codex-ide-mcp-tools)

;;; Tool schema

(defun codex-ide-mcp--arg-type-name (type)
  "Return JSON schema type name for TYPE."
  (pcase type
    ('integer "integer")
    ('number "number")
    ('boolean "boolean")
    (_ "string")))

(defun codex-ide-mcp--arg->schema-property (arg)
  "Return (NAME . SCHEMA) for ARG."
  (cons (plist-get arg :name)
        (delq nil
              (list (cons "type" (codex-ide-mcp--arg-type-name
                                   (plist-get arg :type)))
                    (and-let* ((description (plist-get arg :description)))
                      (cons "description" description))))))

(defun codex-ide-mcp--tool->mcp (tool)
  "Return MCP tool schema for TOOL."
  (let* ((args (plist-get tool :args))
         (properties (mapcar #'codex-ide-mcp--arg->schema-property args))
         (required (cl-loop for arg in args
                            unless (plist-get arg :optional)
                            collect (plist-get arg :name))))
    (delq nil
          (list (cons "name" (plist-get tool :name))
                (cons "description" (plist-get tool :description))
                (cons "inputSchema"
                      (list (cons "type" "object")
                            (cons "properties"
                                  (or properties (make-hash-table :test 'equal)))
                            (cons "required" (vconcat required))
                            (cons "additionalProperties" :json-false)))
                (and-let* ((annotations (plist-get tool :annotations)))
                  (cons "annotations" annotations))))))

(defun codex-ide-mcp--validate-required-args (tool args)
  "Signal `user-error' when TOOL required arguments are absent from ARGS."
  (dolist (arg (plist-get tool :args))
    (when (and (not (plist-get arg :optional))
               (not (codex-ide-mcp--object-has-key-p
                     args (plist-get arg :name))))
      (user-error "Tool %s requires argument %s"
                  (plist-get tool :name)
                  (plist-get arg :name)))))

;;; JSON-RPC dispatch

(defun codex-ide-mcp--handle-initialize (_params)
  "Return MCP initialize result."
  (list (cons "protocolVersion" codex-ide-mcp--protocol-version)
        (cons "capabilities"
              (list (cons "tools"
                          (list (cons "listChanged" :json-false)))))
        (cons "serverInfo"
              (list (cons "name" "emacs-codex")
                    (cons "version" "0.1.1")))))

(defun codex-ide-mcp--handle-tools-list (_params)
  "Return MCP tools/list result."
  (list (cons "tools"
              (vconcat
               (mapcar #'codex-ide-mcp--tool->mcp
                       codex-ide-mcp--tools)))))

(defun codex-ide-mcp--handle-tools-call (params)
  "Call an Emacs MCP tool described by PARAMS."
  (let* ((name (codex-ide-mcp--object-get params "name"))
         (args (or (codex-ide-mcp--object-get params "arguments") nil))
         (tool (and (stringp name) (codex-ide-mcp--tool-by-name name))))
    (unless tool
      (user-error "Unknown MCP tool: %s" name))
    (condition-case err
        (progn
          (codex-ide-mcp--validate-required-args tool args)
          (funcall (plist-get tool :function) args))
      (error
       (codex-ide-mcp--text-error-result (error-message-string err))))))

(defun codex-ide-mcp--dispatch (method params)
  "Dispatch JSON-RPC METHOD with PARAMS and return its result object."
  (pcase method
    ("initialize" (codex-ide-mcp--handle-initialize params))
    ("tools/list" (codex-ide-mcp--handle-tools-list params))
    ("tools/call" (codex-ide-mcp--handle-tools-call params))
    (_ (user-error "Unsupported MCP method: %s" method))))

(defun codex-ide-mcp--valid-id-p (id)
  "Return non-nil when ID is a supported JSON-RPC identifier."
  (or (stringp id) (numberp id)))

(defun codex-ide-mcp--message-kind (message)
  "Return the JSON-RPC kind of MESSAGE, or nil when malformed."
  (when (equal (codex-ide-mcp--object-get message "jsonrpc") "2.0")
    (let ((has-id (codex-ide-mcp--object-has-key-p message "id"))
          (id (codex-ide-mcp--object-get message "id"))
          (method (codex-ide-mcp--object-get message "method"))
          (has-result (codex-ide-mcp--object-has-key-p message "result"))
          (has-error (codex-ide-mcp--object-has-key-p message "error")))
      (cond
       ((and has-id (codex-ide-mcp--valid-id-p id) (stringp method)
             (not has-result) (not has-error))
        'request)
       ((and (not has-id) (stringp method) (not has-result) (not has-error))
        'notification)
       ((and has-id (codex-ide-mcp--valid-id-p id) (not method)
             (not (eq has-result has-error)))
        'response)))))

(defun codex-ide-mcp--handle-message (message)
  "Handle decoded JSON-RPC MESSAGE.
Returns a response alist, or `accepted' for notifications and responses."
  (let ((id (codex-ide-mcp--object-get message "id"))
        (method (codex-ide-mcp--object-get message "method"))
        (params (codex-ide-mcp--object-get message "params")))
    (pcase (codex-ide-mcp--message-kind message)
      ((or 'notification 'response) 'accepted)
      ('request
       (condition-case err
           (codex-ide-mcp--make-response
            id (codex-ide-mcp--dispatch method params))
         (user-error
          (codex-ide-mcp--make-error-response
           id -32601 (error-message-string err)))
         (error
          (codex-ide-mcp--make-error-response
           id -32603 (error-message-string err)))))
      (_ (codex-ide-mcp--make-error-response
          (and (codex-ide-mcp--valid-id-p id) id)
          -32600 "Invalid Request")))))

(provide 'codex-ide-mcp-protocol)

;;; codex-ide-mcp-protocol.el ends here
