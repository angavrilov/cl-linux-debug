(in-package :gtk)

#| The original definition is missing the boolean parameter. |#

(defcfun (tree-view-expand-row "gtk_tree_view_expand_row") :boolean
  (tree-view g-object)
  (path (g-boxed-foreign tree-path))
  (open-all :boolean))
