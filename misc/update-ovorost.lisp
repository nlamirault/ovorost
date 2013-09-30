#!/home/nlamirault/bin/sbcl-git --script


(load (merge-pathnames "src/ovorost/misc/loading.lisp"
                                      (user-homedir-pathname)))

(let ((ovorost (ovorost-web::make-ovorost 'vh-prod)))
  (ovorost-web:update ovorost t))

