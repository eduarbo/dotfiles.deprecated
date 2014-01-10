(require 'key-chord)

(key-chord-define-global "BB" 'iswitchb)
(key-chord-define-global "FF" 'find-file)
(key-chord-define-global "DD" 'dired-jump)
(key-chord-define-global "jk" 'beginning-of-buffer)
(key-chord-define-global "JJ" 'switch-to-previous-buffer)

(key-chord-mode +1)
