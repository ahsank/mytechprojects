(require 'android-defs)
(activity hello
  (on-create-view
   (define counter ::integer 0)
   (define counter-view
     (TextView text: "Not clicked yet."))
   (LinearLayout orientation: LinearLayout:VERTICAL
		 (TextView text: "Hello, Android from Kawa!")
		 (Button
		  text: "Click here!"
		  on-click-listener:
		  (lambda(e)
		    (set! counter (+ counter 1))
		    (counter-view:setText
		     (format "Clicked ~d times." counter))))
		 counter-view)))