FROM node:13

# Install Firebase
RUN npm install -g firebase-tools

# Copy pre-initialized firebase configs
RUN mkdir -p $GITHUB_WORKSPACE/doc/public/dev
ADD .firebaserc $GITHUB_WORKSPACE/doc/
ADD firebase.json $GITHUB_WORKSPACE/doc/
ADD gen.sh $GITHUB_WORKSPACE/

ENTRYPOINT ["$GITHUB_WORKSPACE/gen.sh"]
