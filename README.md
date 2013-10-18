## What is this?

This program makes your CSS/SCSS coding faster and easier than ever. You can see all selectors at once powerd by `helm.el` or `anything.el`. 
 
If you prefer `anything.el` consider below `helm`s as `anything`.

### Install

  1. Clone the `helm-css-scss` repository to some directory:
    ```
    $ git clone https://github.com/xxx/xxx /path/to/helm/directory
    ```
  2. Add to `~/.emacs.el` or `~/.emacs.d/init.el`:

    ```elisp
    (require 'helm-config) ;; https://github.com/emacs-helm/helm
    (add-to-list 'load-path "/path/to/helm/directory")
    (require 'helm-css-scss)
    ```
  3. Restart Emacs or evaluate the above S-expression.

### Usage

#### `helm-css-scss`

Show all selectors and choose one of those and then move to where there is. This function show a few more options pressing `TAB` key when you choose a selector.

#### `helm-css-scss-insert-close-comment`

Insert comment the next of a close brace. If each comment is already there, it will be overwritten. 
In SCSS you can specify a nest depth value in advance: 
`(setq helm-css-scss-insert-close-comment-depth 2)` 
Or on the spot:
`C-u 2 M-x helm-css-scss-insert-close-comment` 
