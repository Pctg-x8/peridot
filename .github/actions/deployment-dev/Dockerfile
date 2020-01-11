FROM node:13

# Install Firebase
RUN npm install -g firebase-tools

# Copy pre-initialized firebase configs
RUN mkdir -p /doc/public/dev
ADD .firebaserc /doc/
ADD firebase.json /doc/
ADD gen.sh /

ENTRYPOINT ["/gen.sh"]
