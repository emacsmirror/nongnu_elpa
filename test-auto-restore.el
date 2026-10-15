;;; test-auto-restore.el --- Test auto-restore behavior for tabspaces -*- lexical-binding: t; -*-

;; This file tests the auto-restore behavior for per-project sessions.
;; The fix ensures that `tabspaces-session-auto-restore` is respected
;; for both global and per-project session restoration.

;; Test Setup Instructions:
;; ========================

;; Test 0: Context-aware manual restore
;; -------------------------------------
;; When tabspaces-session-project-session-store is set to 'project:
;;
;; 1. In a PROJECT tab:
;;    - M-x tabspaces-restore-session (with no arguments)
;;    - Restores the CURRENT PROJECT's session
;;
;; 2. In a NON-PROJECT tab:
;;    - M-x tabspaces-restore-session (with no arguments)  
;;    - Restores the GLOBAL session
;;
;; 3. From anywhere with explicit argument:
;;    - (tabspaces-restore-session "/path/to/project") 
;;    - Restores the specified project's session
;;    - (tabspaces-restore-session "/path/to/session.el")
;;    - Restores the specified session file

;; Test 1: Auto-restore DISABLED (should NOT auto-restore project sessions)
;; -------------------------------------------------------------------------
;; 1. In your init file, set:
;;    (setq tabspaces-session t)
;;    (setq tabspaces-session-project-session-store 'project)
;;    (setq tabspaces-session-auto-restore nil)  ; <-- Key setting
;;
;; 2. Open a project with tabspaces-open-or-create-project-and-workspace
;; 3. Open some files in the project
;; 4. Exit Emacs (this saves the project session)
;; 5. Restart Emacs
;; 6. Open the same project again
;; 7. Expected: Project opens WITHOUT restoring the saved session
;;    - You should see the default project view (e.g., magit-status)
;;    - Previously opened buffers should NOT be restored
;;
;; 8. You can manually restore with: (tabspaces-restore-session "/path/to/project")

;; Test 2: Auto-restore ENABLED (should auto-restore project sessions)
;; --------------------------------------------------------------------
;; 1. In your init file, set:
;;    (setq tabspaces-session t)
;;    (setq tabspaces-session-project-session-store 'project)
;;    (setq tabspaces-session-auto-restore t)  ; <-- Key setting
;;
;; 2. Open a project with tabspaces-open-or-create-project-and-workspace
;; 3. Open some files in the project
;; 4. Exit Emacs (this saves the project session)
;; 5. Restart Emacs
;; 6. Open the same project again
;; 7. Expected: Project opens WITH the saved session restored
;;    - Previously opened buffers should be restored
;;    - Window configuration should be restored
;;
;; Test 3: Global session with auto-restore disabled
;; --------------------------------------------------
;; 1. In your init file, set:
;;    (setq tabspaces-session t)
;;    (setq tabspaces-session-project-session-store nil)  ; Use global
;;    (setq tabspaces-session-auto-restore nil)
;;
;; 2. Open multiple tabs (project and non-project)
;; 3. Exit and restart Emacs
;; 4. Expected: Tabs are NOT auto-restored on startup
;; 5. You can manually restore with: (tabspaces-restore-session)

;; Implementation Note:
;; ===================
;; The fix modifies tabspaces-open-or-create-project-and-workspace
;; at line 769-770 to check tabspaces-session-auto-restore:
;;
;; OLD:
;;   (if (file-exists-p session)
;;       (tabspaces-restore-session session)
;;     (project-switch-project project))
;;
;; NEW:
;;   (if (and tabspaces-session-auto-restore
;;            (file-exists-p session))
;;       (tabspaces-restore-session session)
;;     (project-switch-project project))
;;
;; This ensures consistent behavior between global and per-project
;; session restoration with respect to the auto-restore setting.

(provide 'test-auto-restore)
;;; test-auto-restore.el ends here