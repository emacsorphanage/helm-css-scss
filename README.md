This program makes your CSS/SCSS/LESS coding faster and easier than ever. You can see all selectors at once powered by `helm.el`. 

![helm-css-scss](https://raw2.github.com/ShingoFukuyama/images/master/helm-css-scss.gif)

### Usage

#### `helm-css-scss`

Show all selectors and choose one of those and then move to where it is. This function shows a few more options pressing `TAB` key when you choose a selector. 

While you are choosing any selector from selectors list up and down, synchronize your buffer's cursor position. You'll be like it.

#### `helm-css-scss-multi`

List all CSS/SCSS/LESS selectors within currently opened buffers. It's able to switch the buffers, and view those selectores.

#### `helm-css-scss-insert-close-comment`

Insert comment the next of a close brace. If each comment is already there, it will be overwritten.

### Example config

```elisp
;; helm from https://github.com/emacs-helm/helm
(add-to-list 'load-path "~/.emacs.d/elisp/helm")
(require 'helm)

(add-to-list 'load-path "~/.emacs.d/elisp/helm-css-scss")
(require 'helm-css-scss)

;; Allow comment inserting depth at each end of a brace
(setq helm-css-scss-insert-close-comment-depth 2)
;; If this value is t, split window appears inside the current window
(setq helm-css-scss-split-with-multiple-windows nil)
;; Split direction. 'split-window-vertically or 'split-window-horizontally
(setq helm-css-scss-split-direction 'split-window-vertically)

;; Set local keybind map for css-mode / scss-mode / less-css-mode
(dolist ($hook '(css-mode-hook scss-mode-hook less-css-mode-hook))
  (add-hook
   $hook (lambda ()
           (local-set-key (kbd "s-i") 'helm-css-scss)
           (local-set-key (kbd "s-I") 'helm-css-scss-back-to-last-point))))

(define-key isearch-mode-map (kbd "s-i") 'helm-css-scss-from-isearch)
(define-key helm-css-scss-map (kbd "s-i") 'helm-css-scss-multi-from-helm-css-scss)
```

### Environment

I've confirmed working this program under the following environment.

* Ubuntu 12.04 LTS  with Emacs version 24.3.1
* Mac OSX 10.7.5 with Cocoa Emacs version 24.3.1
* Mac OSX 10.8.5 with Cocoa Emacs version 24.3.1

And each environment with following external elisp. 
[helm.el](https://github.com/emacs-helm/helm)  version 20131016 from Melpa or Github 
