FROM debian:stretch AS debian

ARG DEBIAN_FRONTEND=noninteractive
ARG TERM=linux

RUN echo 'APT::Get::Install-Recommends "false";' > /etc/apt/apt.conf.d/00-general \
    && echo 'APT::Get::Install-Suggests "false";' >> /etc/apt/apt.conf.d/00-general \
    && echo 'APT::Get::Assume-Yes "true";' >> /etc/apt/apt.conf.d/00-general \
    && echo 'APT::Get::force-yes "true";' >> /etc/apt/apt.conf.d/00-general

RUN echo "Europe/Berlin" > /etc/timezone \
    && dpkg-reconfigure -f noninteractive tzdata

RUN apt-get update -q \
    && apt-get install --no-install-recommends -q -y \
         locales \
    && rm -rf /var/lib/apt/lists/* \
	  && localedef -i en_US -c -f UTF-8 -A /usr/share/locale/locale.alias en_US.UTF-8

ENV LANG en_US.utf8


FROM debian AS roswell

RUN apt-get update -q \
    && apt-get install --no-install-recommends -q -y \
         bzip2 \
         ca-certificates curl libcurl3-gnutls \
         make \
    && rm -rf /var/lib/apt/lists/* \
    && curl -L -O https://github.com/roswell/roswell/releases/download/v19.4.10.98/roswell_19.4.10.98-1_amd64.deb \
    && dpkg -i roswell_19.4.10.98-1_amd64.deb \
    && ros setup \
    && rm roswell_19.4.10.98-1_amd64.deb

RUN echo 'export PATH=$HOME/.roswell/bin:$PATH' >> ~/.bashrc


FROM roswell AS builder

RUN apt-get update -q \
    && apt-get install --no-install-recommends -q -y \
         build-essential \
         expect \
         libev-dev \
    && rm -rf /var/lib/apt/lists/*

COPY radiance-bootstrap.lisp radiance-bootstrap.sh radiance-bootstrap-2.sh ./

RUN /usr/bin/expect -f radiance-bootstrap.sh

RUN rm /woo/config/default/i-hunchentoot/* \
    && rmdir /woo/config/default/i-hunchentoot
COPY config/default/i-postmodern/i-postmodern.conf.lisp /woo/config/default/i-postmodern/i-postmodern.conf.lisp
COPY config/default/i-woo/i-woo.conf.lisp /woo/config/default/i-woo/i-woo.conf.lisp
COPY config/default/radiance-core/radiance-core.conf.lisp /woo/config/default/radiance-core/radiance-core.conf.lisp
COPY modules/tfb /woo/modules/tfb

RUN /usr/bin/expect -f radiance-bootstrap-2.sh

RUN rm /woo/quicklisp/dists/quicklisp/archives/* \
    && rm /woo/quicklisp/dists/shirakumo/archives/* \
    && rm /woo/quicklisp/tmp/*


FROM roswell

RUN apt-get update -q \
    && apt-get install --no-install-recommends -q -y \
         libev4 \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /woo

COPY --from=builder /root/.cache/common-lisp /root/.cache/common-lisp
COPY --from=builder /woo /woo

EXPOSE 8080

CMD ros run -- --script start.lisp
