
export DEV_EMAIL="nbuduroi@gmail.com"

# PATH

export M2_HOME=/opt/maven/
export M2=$M2_HOME/bin
export PATH=$PATH:~/bin:~/.lein/bin:$M2:/usr/bin/vendor_perl

# ClojureScript

export CLOJURESCRIPT_HOME=$HOME/source/clojurescript

export CLASSPATH=$CLOJURESCRIPT_HOME/lib/*:$CLOJURESCRIPT_HOME/src/clj:$CLOJURESCRIPT_HOME/src/cljs

# Default Browser

if [ -n "$DISPLAY" ]; then
    export BROWSER=chromium
fi

# Private Stuff

PRIVATE=~/.profile.local
[[ -f $PRIVATE ]] && . $PRIVATE
