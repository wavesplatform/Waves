FROM ubuntu:14.04

RUN apt-get update &&\
    apt-get -yy install python-twisted unzip wget vim git less dc bc software-properties-common &&\
    add-apt-repository -y ppa:webupd8team/java &&\
    apt-get update &&\
    echo "oracle-java8-installer shared/accepted-oracle-license-v1-1 select true" | debconf-set-selections &&\
    apt-get install -y oracle-java8-installer &&\
    apt-get autoclean 				&&\
    apt-get autoremove 				&&\
    rm -rf /var/lib/apt/lists/*

WORKDIR /tmp/

RUN git clone https://github.com/grondilu/bitcoin-bash-tools.git

RUN wget https://github.com/wavesplatform/Waves/releases/download/v0.1.3/waves-testnet-0.1.3.zip &&\
    unzip waves-testnet-0.1.3.zip &&\
    dpkg -i waves-testnet-0.1.3/waves_0.1.3_all.deb

ADD initAndStart.sh /tmp/initAndStart.sh
RUN chmod u+x /tmp/initAndStart.sh

EXPOSE 6869 6868

CMD /tmp/initAndStart.sh
