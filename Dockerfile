FROM centos:7

MAINTAINER Akos Fabian

LABEL Description="This image is used for developing in a Haskell Stack"

ENV USER_NAME=akos
ENV BASE_PATH /home/${USER_NAME}/haskell

RUN yum -y upgrade && \
    yum -y install mc vim tree bash-completion
RUN curl -sSL https://s3.amazonaws.com/download.fpcomplete.com/centos/7/fpco.repo | tee /etc/yum.repos.d/fpco.repo
RUN yum -y install stack
RUN adduser ${USER_NAME}
RUN mkdir ${BASE_PATH}
RUN chown ${USER_NAME}: ${BASE_PATH}
RUN su - ${USER_NAME} -c "stack setup --resolver ghc-7.8.4"

VOLUME ${BASE_PATH}/shared

USER ${USER_NAME}
WORKDIR ${BASE_PATH}
