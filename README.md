## What is this?

This program makes your CSS/SCSS coding faster and easier than ever. You can see all selectors at once powerd by `helm.el` or `anything.el`. 

[Quick look! 48 second Youtube live coding](http://www.youtube.com/watch?v=25Fqi-4WL4o&feature=player_embedded)

![helm-css-scss](https://github.com/ShingoFukuyama/helm-css-scss/raw/master/image/helm-css-scss.png)

If you prefer `anything.el` just consider below `helm`s as `anything`.

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

Add exciting feature! While you choosing any selector from selectors list up and down, synchronize your buffer's cursor position. You'll be like it.


#### `helm-css-scss-insert-close-comment`

Insert comment the next of a close brace. If each comment is already there, it will be overwritten. 
In SCSS you can specify a nest depth value in advance: 

`(setq helm-css-scss-insert-close-comment-depth 2)` 

Or on the spot:

`C-u 2 M-x helm-css-scss-insert-close-comment`

### Example config

```elisp
(require 'helm-config) ;; https://github.com/emacs-helm/helm
(add-to-list 'load-path
             (concat "~/.emacs.d/elisp/helm-css-scss"))
(require 'helm-css-scss)

;; Allow comment inserting depth at each end of a brace
(setq helm-css-scss-insert-close-comment-depth 2)

;; Set local keybind map for css-mode / scss-mode
(dolist ($hook '(css-mode-hook scss-mode-hook))
  (add-hook
   $hook
   (lambda ()
     (local-set-key (kbd "s-i") ;; [command + i]
                    'helm-css-scss)
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
`helm.el`  version 20130606.946  
`anything` version 20131016.820
