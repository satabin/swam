#! /usr/bin/env bash

set -eux

echo $GPG_PRIVATE_KEY | base64 --decode > gpg_key

gpg --import gpg_key

rm gpg_key

mill mill.scalalib.PublishModule/publishAll --sonatypeCreds "$SONATYPE_USER:$SONATYPE_PASSWORD" --gpgPassphrase "$GPG_PASSPHRASE" --release true _.publishArtifacts --readTimeout 600000
