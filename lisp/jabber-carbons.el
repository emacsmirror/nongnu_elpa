;;; jabber-carbons.el --- Support for XEP-0280: Message Carbons  -*- lexical-binding: t; -*-

;;; Commentary:
;;

;;; Code:

(require 'jabber-util)
(require 'jabber-xml)
(require 'jabber-menu)
(require 'jabber-iq)
(require 'jabber-disco)

(defconst jabber-carbons-xmlns "urn:xmpp:carbons:2"
  "XML namespace for XEP-0280 Message Carbons.")

(defun jabber-carbon-success (jc xml-data _context)
  (when (equal "result" (jabber-xml-get-attribute xml-data 'type))
    (message "Carbons feature successfully enabled for %s"
             (jabber-connection-jid jc))))

(defun jabber-carbon-failure (_jc xml-data _context)
  (message "Carbons feature could not be enabled: %S" xml-data))

(add-to-list 'jabber-jid-service-menu
             (cons "Enable Carbons" 'jabber-enable-carbons))

;;;###autoload
(defun jabber-enable-carbons (jc)
  "Send request to enable XEP-0280 Message Carbons.

JC is the Jabber connection."
  (interactive (list (jabber-read-account)))
  (jabber-send-iq jc
                  nil
                  "set"
                  `(enable ((xmlns . ,jabber-carbons-xmlns)))
                  #'jabber-carbon-success "Carbons feature enablement"
                  #'jabber-carbon-failure "Carbons feature enablement"))

(jabber-disco-advertise-feature jabber-carbons-xmlns)

(provide 'jabber-carbons)

;;; jabber-carbons.el ends here
