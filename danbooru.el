(defvar danbooru-dir "~/danbooru"
  "Directory to find Danbooru-specific bbcode files")

(defun danbooru-set-bbcode-dir (dirname &optional switches)
  (interactive (dired-read-dir-and-switches ""))
  (setq danbooru-dir dirname))

(defun danbooru-bbcode-load-file (name)
  "Load/create a bbcode file NAME. It looks for bbcode files under the directory
defined in `danbooru-dir'."
  (interactive
   (list
    (completing-read
     "File: "
     (remove "."
	     (remove ".."
		     (directory-files danbooru-dir))))))
  (find-file (format "%s/%s" danbooru-dir name)))
