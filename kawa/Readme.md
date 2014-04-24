Running Kawa
====================
```bash
export CLASSPATH=/Users/ahsank/packages/java/lib/kawa-1.14.jar
java kawa/repl
#|kawa:1|#(exit)
```
Kawa on android
=========================
cd =/Users/ahsank/packages/android
svn -q checkout svn://sourceware.org/svn/kawa/trunk kawa
# Or get Kawa.1.14.tar.gz from the kawa web site
KAWA_DIR=/Users/ahsank/packages/android/kawa
cd kawa

export ANDROID_HOME=/Users/ahsank/Development/adt-bundle/sdk
PATH=$ANDROID_HOME/tools:$ANDROID_HOME/platform-tools:$PATH
ANDROID_PLATFORM=android-18
$ ./configure --with-android=$ANDROID_HOME/platforms/$ANDROID_PLATFORM/android.jar --disable-xquery --disable-jemacs
$ make
# ant -Denable-android=true doesn't work


Create project
----------------
PROJECT_DIR=KawaHello
PROJECT_CLASS=hello
PROJECT_PACKAGE=kawa.android
PROJECT_PACKAGE_PATH=kawa/android

cd  $KAWA_DIR
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

First you need to create an Android Virtual Device (avd). Start:
android
Then from menu Tools select Manage AVDs.... In the new window click New.... Pick a Name (we use avd16 in the following), a Target (to match $ANDROID_PLATFORM), and optionally change the other properties, before clicking Create AVD.
Now you can start up the Android emulator:

emulator -avd avd18 &
adb install bin/KawaHello-debug.apk
To debug:
adb logcat


custom_rules.xml:

<?xml version="1.0" encoding="UTF-8"?>
<project name="kawa_rules" default="help">
   <target name="-post-compile" depends="scompile">
   </target>

   <!-- Compile this project's .scm files into .class files. -->
   <target name="scompile" depends="-compile">
      <xpath input="${manifest.abs.file}" expression="/manifest/application/activity/@android:name" output="android.activity.name" />
      <propertybyreplace name="project.app.package.path" input="${project.app.package}" replace="." with="/" />
      <java failonerror="true" fork="true" classname="kawa.repl">
         <classpath>
            <pathelement path="libs/kawa.jar" />
            <pathelement path="${sdk.dir}/platforms/${target}/android.jar" />
            <pathelement path="${out.classes.absolute.dir}" />
         </classpath>
         <arg value="-d" />
         <arg path="${out.classes.absolute.dir}" />
         <arg line="-P ${project.app.package}. --warn-undefined-variable --module-static-run --warn-invoke-unknown-method --warn-as-error" />
         <arg value="-C" />
         <arg file="src/${project.app.package.path}/${android.activity.name}.scm" />
      </java>
   </target>
</project>
~