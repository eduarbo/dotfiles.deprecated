# Switchfly shell tools

export SWITCHFLY_HOME=$HOME/dev/switchfly/repos/dev
export TZ="UTC"

# Make sure these env variables are set in ~/.bash_profile
export JAVA_HOME="/usr/lib/jvm/java-7-oracle"
export JDK_HOME=$JAVA_HOME
export ANT_HOME="/user/bin/ant"
export MAVEN_HOME="/usr/share/maven"
export M2_HOME="/usr/share/maven"
export PLATFORM="/usr/local/platform"

# Ant & Maven options
export ANT_OPTS="-Xmx512m -Xms512m"
export MAVEN_OPTS="-Xmx2G -Xms2G -XX:PermSize=512M -XX:MaxPermSize=512m"
export JVM_ARGS="-Xms1024m -Xmx1024m -XX:MaxPermSize=512m"
# export MAVEN_OPTS="-Xmx2G -Xms2G -Xdebug -XX:PermSize=256M -XX:MaxPermSize=512m -Xrunjdwp:transport=dt_socket,address=4000,server=y,suspend=n"

# Aliases
alias sw='cd $SWITCHFLY_HOME'
alias repos='cd ~/dev/switchfly/repos'
alias deploy='cd $SWITCHFLY_HOME/webapp-spring && mvn tomcat7:run'

alias build="mvn clean install -Dmaven.test.skip=true"
alias test="mvn clean install"

alias sw.build='sw; mvn clean install -Dmaven.test.skip=true'
alias sw.test='sw; mvn clean install'
alias sw.run='sw; build; app.start'
alias sw.start='sw; cd core-webapp; mvn clean tomcat7:run'
alias sw.stop='tkill'
alias sw.kill='kill -9 `ps auwx | grep tomcat | grep java | awk '\''{print $2}'\''` || ps auwx | grep tomcat'
alias sw.init='sw; ant initialize_branch_properties'
