(require 'android-defs)
(activity hello
  (on-create-view
   (android.widget.TextView (this)
    text: "Hello, Android from Kawa Scheme!")))
