;;; autoumn-theme.el --- Autumn theme for Emacs

;; Copyright (C) 2011-2014 Jordan Galby
;; Author: Jordan Galby
;; URL: http://github.com/Gravemind/emacs-theme-autumn

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

(deftheme autumn "Low contrast dark autumn theme")

(let (
      (background   "#202020")
      (foreground   "#b0b0b0")
      (cursor       "#00ffff")

      (bg-hl        "#282828")

      (selection    "#404040")

      (comment      "#909090")

      (color1       "#f7bc75")
      (color2       "#d47f54")
      (color3       "#ab5b48")
      (color4       "#8A423f")
      (strings      "#b2ad79")
      (otherkeyw    "#61cfd4")

      (colorrem     "#B95151")
      (coloradd     "#6AAB6A")
      (colorrem-bg  "#493434")
      (coloradd-bg  "#384a36")

      (useless      "#602929")

      (ansi-bold-blue "#4B84B8")
      (ansi-bold-cyan "#71bebe")

      (winframe-border "#354044")
      (winframe-active "#354044")
      (winframe-inacti "#253034")

      )

  (progn

    (setq x-use-underline-position-properties nil)
    (setq underline-minimum-offset 4)

    (custom-theme-set-faces
     'autumn

     `(cursor ((t (:background ,cursor))))

     `(default ((t (:foreground ,foreground))))

     ;; Languages
     `(font-lock-builtin-face ((t (:foreground ,otherkeyw))))
     `(font-lock-preprocessor-face ((t (:foreground ,color4))))
     `(font-lock-constant-face ((t (:foreground ,color3))))
     `(font-lock-function-name-face ((t (:foreground ,color1))))
     `(font-lock-keyword-face ((t (:foreground ,color3 :weight normal))))
     `(font-lock-string-face ((t (:foreground ,strings))))
     `(font-lock-type-face ((t (:foreground ,color2))))
     `(font-lock-variable-name-face ((t (:foreground ,color1))))

     ;; Comments
     `(font-lock-comment-delimiter-face ((t (:foreground ,comment))))
     `(font-lock-comment-face ((t (:foreground ,comment :slant italic))))
     `(shadow ((t (:foreground ,comment))))

     ;; Mode line
     `(mode-line ((t (:background ,winframe-active :foreground "#a0a0a0" :box (:line-width 4 :color ,winframe-active) :weight normal))))
     `(mode-line-inactive ((t (:inherit mode-line :background ,winframe-inacti :foreground "#a0a0a0" :box (:line-width 4 :color ,winframe-inacti) :weight normal))))
     `(mode-line-buffer-id ((t (:foreground "#d0d0d0"))))
     `(mode-line-highlight ((t (:box nil))))
     `(vertical-border ((t (:foreground ,winframe-border))))
     `(window-divider ((t (:foreground ,winframe-border))))
     `(window-divider-first-pixel ((t (:inherit window-divider))))
     `(window-divider-last-pixel ((t (:inherit window-divider))))
     `(fringe ((t (:foreground ,winframe-border :background ,background))))

     ;; White space mode
     `(whitespace-space-before-tab ((t (:background ,useless))))
     `(whitespace-trailing ((t (:background ,useless))))
     `(whitespace-indentation ((t (:background ,useless))))

     `(minibuffer-prompt ((t (:foreground "#36b5b1"))))

     `(linum ((t (:foreground ,winframe-border :background ,background))))
     `(highlight ((t (:background "gray15" :height 1.0 :weight normal))))
     `(mouse ((t (:background "white"))))
     `(region ((t (:background ,selection))))
     `(ac-completion-face ((t (:foreground "white" :underline t))))
     `(popup-isearch-match ((t (:background "sky blue" :foreground "red"))))
     `(semantic-tag-boundary-face ((t (:overline "#303030"))))
     `(font-lock-warning-face ((t (:foreground "#B34949" :weight normal :underline nil))))
     `(compilation-warning ((t (:foreground "#66D466" :weight normal :underline nil))))
     `(compilation-info ((t (:foreground "#79B379" :weight normal :underline nil))))

     `(match ((t (:background nil :foreground "RoyalBlue2"))))

     `(mumamo-background-chunk-major ((t (:background "#202020"))))
     `(mumamo-background-chunk-submode1 ((t (:background "gray10"))))
     `(mumamo-background-chunk-submode2 ((t (:background "gray10"))))
     `(mumamo-background-chunk-submode3 ((t (:background "gray10"))))
     `(mumamo-background-chunk-submode4 ((t (:background "gray10"))))


     `(flymake-errline ((t (:background nil :underline "#CC2222"))))
     `(flymake-warnline ((t (:background nil :underline "#22CC22"))))

     `(rtags-errline ((t (:background nil :underline "#CC2222"))))
     `(rtags-warnline ((t (:background nil :underline "#22CC22"))))
     `(rtags-fixitline ((t (:background nil :underline "#FFBB00"))))

     `(ebrowse-root-class ((t (:foreground "#f1aa7e" :weight normal ))))
     `(ebrowse-member-class ((t (:foreground "#f1aa7e" :weight normal ))))

     `(buffer-menu-star-buffer ((t (:foreground ,comment :slant normal))))
     `(buffer-menu-buffer-name ((t (:foreground ,color2 :weight normal))))
     `(buffer-menu-read-only-mark ((t (:foreground ,strings))))
     `(buffer-menu-directory-buffer ((t (:foreground ,color3 :background nil))))

     `(buffer-menu-mode ((t (:foreground ,color3))))
     `(buffer-menu-file-name ((t (:foreground ,color3))))
     `(buffer-menu-modified-mark ((t (:foreground ,strings))))
     `(buffer-menu-size ((t (:foreground ,color3))))

     `(git-gutter-fr:deleted  ((t (:foreground "#722"))))
     `(git-gutter-fr:added    ((t (:foreground "#393"))))
     `(git-gutter-fr:modified ((t (:foreground "#CA0"))))
     `(git-gutter-fr+-deleted  ((t (:foreground "#722"))))
     `(git-gutter-fr+-added    ((t (:foreground "#393"))))
     `(git-gutter-fr+-modified ((t (:foreground "#CA0"))))

     `(diff-hl-delete ((t (:foreground "#853434"))))
     `(diff-hl-insert ((t (:foreground "#3B7C3B"))))
     `(diff-hl-change ((t (:foreground "#45517C"))))

     `(dired-ignored ((t (:inherit shadow :foreground "DimGray"))))
     `(dired-k-added ((t (:foreground ,coloradd :weight bold))))
     `(dired-k-commited ((t (:foreground ,coloradd :weight bold))))
     `(dired-k-directory ((t nil)))
     `(dired-k-modified ((t (:foreground ,colorrem :weight bold))))
     `(dired-k-untracked ((t (:foreground "#8B3D3D" :weight bold))))

     ;;
     ;; diff
     ;;
     `(diff-context ((t nil)))
     `(diff-file-header ((t (:background "#aaa" :foreground "#000" :weight bold))))
     `(diff-function ((t (:inherit diff-file-header))))
     `(diff-header ((t (:background "#aaa" :foreground "#555" :weight bold))))

     `(diff-indicator-removed ((t (:background ,colorrem :foreground ,background))))
     `(diff-indicator-added   ((t (:background ,coloradd :foreground ,background))))

     `(diff-removed ((t (:background nil :foreground ,colorrem))))
     `(diff-added ((t (:background nil :foreground ,coloradd))))
     `(diff-refine-removed ((t (:background ,colorrem-bg))))
     `(diff-refine-added   ((t (:background ,coloradd-bg))))

     `(magit-item-highlight ((t (:background ,bg-hl))))
     `(magit-section-highlight ((t (:background ,bg-hl))))
     `(magit-diff-context ((t nil)))
     `(magit-diff-context-highlight ((t (:background ,bg-hl))))
     `(magit-diff-file-heading ((t (:background "#000" :foreground "#bbb"))))
     `(magit-diff-file-heading-highlight ((t (:inherit magit-file-heading :background "#111"))))
     `(magit-diff-hunk-heading ((t (:background "#333" :foreground "#ddd"))))
     `(magit-diff-hunk-heading-highlight ((t (:background "#444" :foreground "#ddd"))))

     `(magit-diff-none ((nil ())))
     ;`(magit-diff-removed-highlight ((t (:background ,background :foreground ,colorrem))))
     ;`(magit-diff-added-highlight ((t (:background ,background :foreground ,coloradd))))
     `(magit-diff-removed-highlight ((t (:background ,bg-hl :foreground ,colorrem))))
     `(magit-diff-added-highlight ((t (:background ,bg-hl :foreground ,coloradd))))
     `(magit-diff-removed ((t (:foreground ,colorrem))))
     `(magit-diff-added ((t (:foreground ,coloradd))))

     `(smerge-mine ((t (:background nil))))
     `(smerge-lower ((t (:background nil))))
     `(smerge-base ((t (:background nil))))
     `(smerge-other ((t (:background nil))))
     `(smerge-upper ((t (:background nil))))

     `(smerge-refined-removed ((t (:background ,colorrem-bg))))
     `(smerge-refined-added   ((t (:background ,coloradd-bg))))

     ;;
     ;; magit
     ;;
     `(magit-branch-local ((t (:foreground "DarkSeaGreen2"))))
     `(magit-branch-remote ((t (:foreground "LightSkyBlue1"))))
     ;; disable remote head branch face, make it like any other remote branch
     `(magit-branch-remote-head ((t (:inherit magit-branch-remote))))

     ;;
     ;; auto-highlight-symbol
     ;;

     `(ahs-plugin-defalt-face ((t (:foreground nil :background nil :underline "cyan"))))
     `(ahs-face ((t (:foreground nil :background nil :underline "Orange1"))))
     `(ahs-definition-face ((t (:foreground nil :background nil :underline t))))

     ;;
     ;; rtags
     ;;
     `(rtags-skippedline ((t (:background nil))))

     ;;
     ;; Man
     ;;
     `(Man-overstrike ((t (:foreground ,ansi-bold-blue :weight bold))))
     `(Man-underline ((t (:foreground ,ansi-bold-cyan :weight bold))))

     ;;
     ;; ido
     ;;
     `(ido-first-match ((t (:foreground ,otherkeyw))))
     `(ido-only-match ((t (:foreground ,otherkeyw))))
     `(ido-virtual ((t (:foreground ,comment))))
     `(ido-subdir ((t nil)))

     ;;
     ;; helm
     ;;
     `(helm-candidate-number ((t nil)))
     `(helm-header ((t (:foreground ,color3 :slant italic))))
     `(helm-match ((t (:foreground ,color1))))
     `(helm-selection ((t (:background ,selection))))
     `(helm-source-header ((t (:foreground ,color2))))

     `(helm-grep-file ((t (:inherit compilation-info))))
     `(helm-grep-finish ((t nil)))
     `(helm-grep-lineno ((t (:inherit compilation-line-number))))

     ;; history
     `(history-prompt ((t (:inherit minibuffer-prompt))))
     `(history-current-history ((t (:background ,cursor :foreground ,background))))
     `(history-other-history ((t nil)))

     ;; visible-mark
     `(visible-mark-face1 ((t (:background "#080808" :foreground nil))))
     `(visible-mark-face2 ((t (:background "#181818" :foreground nil))))

     )

    (if (window-system)
        (set-face-background 'default background))

    ;; if we are in a windowed emacs, set the backgroud, else let it transparent for terminals
    (add-hook 'after-make-frame-functions
              (lambda (frame)
                (select-frame frame)
                (if (window-system frame)
                    (set-face-background 'default "#202020"))
                ))

    (setq-default ansi-color-names-vector
                  ["#666666" "#cf6171" "#B8EB6C" "#fff796" "#4B84B8" "#cf9ebe" "#71bebe" "#ffffff"]
                  )

    ;; #9CABBA
    ;; #A59FA8
    ;; #AE9397
    ;; #B88886
    ;; #C17C75
    ;; #CB7164
    ;; #D46553
    ;; #DD5942
    ;; #E74E31
    ;; #F04220
    ;; #FA370F
    (setq-default dired-k-size-colors
                  '((10000     . "#9CABBA") ; 10K
                    (50000     . "#AE9397") ; 50K
                    (100000    . "#B88886") ; 100K
                    (1000000   . "#CB7164") ; 1M
                    (10000000  . "#DD5942") ; 10M
                    (100000000 . "#F04220") ; 100M
                    )
                  dired-k-date-colors
                  '((0        . "red")     ; 0
                    (86400    . "#d0d0d0") ; 1day
                    (604800   . "#b0b0b0") ; 1week
                    (2419200  . "#909090") ; 4weeks
                    (15778800 . "#707070") ; 6months
                    (31557600 . "#505050") ; 1year
                    )
                  )

    ;; (setq buffer-menu-buffer-font-lock-keywords
    ;;     '(("^....[*]Man .*Man.*" . font-lock-variable-name-face) ; Man page
    ;;       (".*Dired.*"       . font-lock-comment-face)   ; Dired
    ;;       ("^....[*]shell.*"   . font-lock-preprocessor-face)  ; shell buff
    ;;       (".*[*]scratch[*].*"   . font-lock-function-name-face) ; scratch buffer
    ;;       ("^....[*].*"      . font-lock-string-face)    ; "*" named buffers
    ;;       ("^..[*].*"        . font-lock-constant-face)    ; Modified
    ;;       ("^.[%].*"       . font-lock-keyword-face)))   ; Read only
    ;; (defun buffer-menu-custom-font-lock  ()
    ;;   (let ((font-lock-unfontify-region-function
    ;;      (lambda (start end)
    ;;        (remove-text-properties start end '(font-lock-face nil)))))
    ;;     (font-lock-unfontify-buffer)
    ;;     (set (make-local-variable 'font-lock-defaults)
    ;;      '(buffer-menu-buffer-font-lock-keywords t))
    ;;     (font-lock-fontify-buffer)))
    ;; (add-hook 'buffer-menu-mode-hook 'buffer-menu-custom-font-lock)
    ;; (add-hook 'electric-buffer-menu-mode-hook 'buffer-menu-custom-font-lock)
    )
  )

(provide-theme 'autumn)

;;EOF
