This program makes your CSS/SCSS coding faster and easier than ever. You can see all selectors at once powerd by `helm.el`. 

[48 seconds Youtube live coding](http://www.youtube.com/embed/25Fqi-4WL4o?rel=0)

![helm-css-scss](https://github.com/ShingoFukuyama/helm-css-scss/raw/master/image/helm-css-scss.png)

### Install

  1. Clone the `helm-css-scss` repository to some directory:
    ```
    $ git clone https://github.com/ShingoFukuyama/helm-css-scss.git
    ```
  2. Add to `~/.emacs.el` or `~/.emacs.d/init.el`:

    ```elisp
    (require 'helm-config) ;; https://github.com/emacs-helm/helm
    (add-to-list 'load-path "/path/to/helm-css-scss")
    (require 'helm-css-scss)
    ```
  3. Restart Emacs or evaluate the above S-expression.

### Usage

#### `helm-css-scss`

Show all selectors and choose one of those and then move to where it is. This function show a few more options pressing `TAB` key when you choose a selector. 

While you choosing any selector from selectors list up and down, synchronize your buffer's cursor position. You'll be like it.

#### `helm-css-scss-insert-close-comment`

Insert comment the next of a close brace. If each comment is already there, it will be overwritten.

#### Configure variable

In default, Helm popup window appears horizontally. If you'd like to split window vertically set this as a t not nil. 

`(setq helm-css-scss-split-window-vertically nil)` 

In SCSS you can specify a nest depth value in advance.

`(setq helm-css-scss-insert-close-comment-depth 2)` 

If it's t not nil, include selectors within commented area.

`(defvar helm-css-scss-include-commented-selector nil` 


### Example config

```elisp
(require 'helm-config)

(add-to-list 'load-path "~/.emacs.d/elisp/helm-css-scss")
(require 'helm-css-scss)

;; Allow comment inserting depth at each end of a brace
(setq helm-css-scss-insert-close-comment-depth 2)

;; nil is horizontally. t is vertically
(setq helm-css-scss-split-window-vertically nil)

;; Set local keybind map for css-mode / scss-mode
(dolist ($hook '(css-mode-hook scss-mode-hook))
  (add-hook
   $hook
   (lambda ()
     (local-set-key (kbd "s-i") ;; [command + i]
                    'helm-css-scss)
     (local-set-key (kbd "s-b") ;; [command + b]
                    'helm-css-scss-back-to-last-point)
     (local-set-key (kbd "s-c") ;; [command + c]
                    'helm-css-scss-insert-close-comment)
     (local-set-key (kbd "s-n") ;; [command + n]
                    'helm-css-scss-move-and-echo-next-selector)
     (local-set-key (kbd "s-p") ;; [command + p]
                    'helm-css-scss-move-and-echo-previous-selector)
           )))
```

### Environment

I've confirmed working this program under the following environment.

* Ubuntu 12.04 LTS  with Emacs version 24.3.1
* Mac OSX 10.7.5 with Cocoa Emacs version 24.3.1
* Mac OSX 10.8.5 with Cocoa Emacs version 24.3.1

And each environment with following external elisp. 
[helm.el](https://github.com/emacs-helm/helm)  version 20131016 from Melpa or Github 

### Other infomation

I use default css-mode and [scss-mode](https://github.com/antonj/scss-mode). 

According to [Google HTML/CSS Style Guide](http://google-styleguide.googlecode.com/svn/trunk/htmlcssguide.xml#CSS_Quotation_Marks), it says Do not use quotation marks in `url()`. But URLs like "http://..." and Data URI schemes which including "//" break the syntax of css-mode and scss-mode so helm-css-scss also affected. 

It's no problem if you wrap those with single or double quotation.

### Anything.el

I'm not sure but helm-css-scss would work the same way when you just replace all "helm" strings in `helm-css-scss.el` to "anything".
