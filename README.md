## What is this?

This program makes your CSS/SCSS coding faster and easier than ever. You can see all selectors at once powerd by `helm.el` or `anything.el`. 

![helm-css-scss](https://github.com/ShingoFukuyama/helm-css-scss/raw/master/image/helm-css-scss.png)

<<<<<<< HEAD
If you prefer `anything.el` just consider below `helm`s as `anything`.
=======
If you prefer `anything.el` consider below `helm`s as `anything`.
>>>>>>> ac74a0aaeaf349308f1f06ab5e700d7e15eeab59

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
