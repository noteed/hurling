FROM ubuntu:12.04

RUN apt-get update
RUN apt-get install -q -y language-pack-en
RUN update-locale LANG=en_US.UTF-8

RUN DEBIAN_FRONTEND=noninteractive apt-get install -q -y git sudo
RUN DEBIAN_FRONTEND=noninteractive apt-get install -q -y apt-transport-https

# Install PostgreSQL
RUN DEBIAN_FRONTEND=noninteractive apt-get install -q -y postgresql-9.1
RUN DEBIAN_FRONTEND=noninteractive apt-get install -q -y libgmp10 libpq5

RUN apt-key adv --keyserver hkp://keyserver.ubuntu.com:80 \
  --recv-keys 36A1D7869245C8950F966E92D8576A8BA88D21E9
RUN echo deb https://get.docker.com/ubuntu docker main > /etc/apt/sources.list.d/docker.list
RUN apt-get update
RUN DEBIAN_FRONTEND=noninteractive apt-get install -q -y lxc-docker-1.3.3

# Configure
RUN sh -c "service postgresql start ; su postgres -c 'createuser -d -r -s docker' ; service postgresql stop"
RUN sh -c "service postgresql start ; su postgres -c \"psql -c \\\"ALTER USER docker PASSWORD 'docker'\\\"\" ; service postgresql stop"
RUN sh -c "service postgresql start ; su postgres -c 'createdb -O docker docker' ; service postgresql stop"
RUN sed -i -e 's/^host *all *all *127.0.0.1\/32 *md5$/host  all  all  0.0.0.0\/0  md5/' /etc/postgresql/9.1/main/pg_hba.conf
RUN echo "listen_addresses = '*'" >> /etc/postgresql/9.1/main/postgresql.conf

ADD run.sh /run.sh
ADD humming /usr/bin/humming
ADD hurling /usr/bin/hurling

CMD /run.sh
