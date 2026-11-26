FROM node:18

# Install tools
RUN npm install -g firebase-tools
RUN apt-get update && apt-get install -y rsync

# Copy pre-initialized firebase configs
RUN mkdir -p /doc/public/dev
ADD .firebaserc /doc/
ADD firebase.json /doc/
ADD gen.sh /

ENTRYPOINT ["/gen.sh"]
