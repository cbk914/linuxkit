FROM ocaml/opam:alpine-3.5_ocaml-4.04.0
RUN opam depext -i -y jbuilder lwt cohttp astring
ADD . /home/opam/src/opam
RUN opam pin add -ny mypkg /home/opam/src
RUN opam install --deps-only mypkg
ENV JBUILD_STATIC=true
WORKDIR /home/opam/src
ADD . /home/opam/src
RUN sudo chown opam . .merlin
RUN opam config exec -- make
RUN sudo cp _build/default/main.exe /usr/bin/https-unikernel
RUN sudo rm -rf /home/opam/src
