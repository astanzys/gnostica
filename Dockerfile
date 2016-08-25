FROM java:8-alpine
MAINTAINER Your Name <you@example.com>

ADD target/uberjar/gnostica.jar /gnostica/app.jar

EXPOSE 3000

CMD ["java", "-jar", "/gnostica/app.jar"]
