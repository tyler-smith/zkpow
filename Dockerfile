FROM ubuntu:18.04

WORKDIR /opt/app

ADD . /snarky

RUN /snarky/scripts/depends.sh && \
  bash <(curl -sL https://raw.githubusercontent.com/ocaml/opam/master/shell/install.sh) && \
  bash <(curl -sL https://raw.githubusercontent.com/o1-labs/snarky/master/scripts/depends.sh) && \
  opam pin add git@github.com:o1-labs/snarky.git

ENTRYPOINT [ "dune" ]