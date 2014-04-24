Running Kawa
====================
```bash
export CLASSPATH=/Users/ahsank/packages/java/lib/kawa-1.14.jar
java kawa/repl
#|kawa:1|#(exit)
```
Kawa on Android
=========================
```bash
cd =/Users/ahsank/packages/android
svn -q checkout svn://sourceware.org/svn/kawa/trunk kawa
# Or get Kawa.1.14.tar.gz from the kawa web site
KAWA_DIR=/Users/ahsank/packages/android/kawa
cd kawa

export ANDROID_HOME=/Users/ahsank/Development/adt-bundle/sdk
PATH=$ANDROID_HOME/tools:$ANDROID_HOME/platform-tools:$PATH
ANDROID_PLATFORM=android-18
./configure --with-android=$ANDROID_HOME/platforms/$ANDROID_PLATFORM/android.jar --disable-xquery --disable-jemacs
$ make
# ant -Denable-android=true doesn't work
```

Create Android Project
---------------------
```bash
PROJECT_DIR=KawaHello
PROJECT_CLASS=hello
PROJECT_PACKAGE=kawa.android
PROJECT_PACKAGE_PATH=kawa/android

cd  project parent dir
android create project --target $ANDROID_PLATFORM --name $PROJECT_DIR --activity $PROJECT_CLASS --path ./$PROJECT_DIR --package $PROJECT_PACKAGE

cd $PROJECT_DIR
HELLO_APP_DIR=`pwd`
cd $HELLO_APP_DIR/src/$PROJECT_PACKAGE_PATH
rm $PROJECT_CLASS.java
cat > $PROJECT_CLASS.scm
(require 'android-defs)
(activity hello
  (on-create-view
   (android.widget.TextView (this)
    text: "Hello, Android from Kawa Scheme!")))

^D

cd $HELLO_APP_DIR
ln -s $KAWA_DIR/kawa-1.14.1.jar libs/kawa.jar
ln -s $KAWA_DIR/gnu/kawa/android/custom_rules.xml .
# If custom_rules.xml doesn't exist then  get it from 
curl -O  https://sourceware.org/svn/kawa/trunk/gnu/kawa/android/custom_rules.xml
If kawa-1.14.1.jar doesn't exist check whether the library version has changed
ant debug
```
Run the code
---------------------
Create Android Virtual Device (avd): Run android command then from menu Tools select Manage AVDs....
In the new window click New.... Pick a Name for example avd18 and fill properties, click Create AVD.
Start up the Android emulator:
```bash
emulator -avd avd18 &
adb install bin/KawaHello-debug.apk
# To debug:
adb logcat
```