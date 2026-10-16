#!/bin/sh

# Builds the release version and pushes dist/ to the repository that serves
# GitHub Pages. Usage: sh ./deploy.sh

REPO=git@github.com:kztk-m/lambda-eval.git
BRANCH=main
DEPLOY=./deploy

# Check that everything has been committed (except this file) and pushed.

if [ -n "$(git status --porcelain -- ':!deploy.sh')" ];
then
    echo "Uncommitted changes detected (excluding deploy.sh):"
    git status --short -- ':!deploy.sh' | sed 's/^/> /'
    exit 1
fi

git fetch -q || exit 1
if [ "$(git rev-parse @)" != "$(git rev-parse @{u})" ];
then
    echo "HEAD differs from its upstream (unpushed commits, or not pulled)."
    exit 1
fi

# Commit Message
COMMIT=$(git rev-parse HEAD)

if test -e $DEPLOY;
then
    echo "$DEPLOY exists. Remove or rename it beforehand."
    exit 1
fi

# Rebuild dist/ from scratch so that removed files do not remain.
rm -rf dist
if ! make release;
then
    echo "make release failed."
    exit 1
fi

CURRENTDIR=`pwd`

git clone -q --depth 1 $REPO $DEPLOY || exit 1

echo "Cleaning up $DEPLOY ..."
# Cleaning up $DEPLOY, while keeping ".git" (use of * is intentional).
rm -rf "$DEPLOY"/*

echo "Copying files ..."
cp -R dist/* $DEPLOY

touch $DEPLOY/.nojekyll

echo "Deploying"
cd $DEPLOY
git add -A .
if git diff --cached --quiet;
then
    echo "Nothing changed; skipping commit and push."
else
    git commit -q -m "generated from $COMMIT" \
        && git push -q origin HEAD:$BRANCH \
        || { echo "Commit or push failed. $DEPLOY is kept for inspection."; exit 1; }
fi

cd $CURRENTDIR
echo "Removing $DEPLOY ..."
rm -rf $DEPLOY

echo "Done."
