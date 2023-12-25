This repository contains Ansible and R scripts to run benchmarks on web servers and plot the data.

# Ansible
There are two Ansible playbooks, one to install the servers and the other one to run the benchmarks,
collect and process the data.

### hosts.ini
The [hosts.ini](hosts.ini) file contains the configurations of the hosts. There is a dev environment
by using _vagrant_ and a prod environment by using _hetzner_.

The dev environment uses [Vagrant](https://www.vagrantup.com/) to test the playbooks, a [Vagrantfile](Vagrantfile)
is available in the repository to create the VMs.

The prod environment needs additional settings like specifying the IP addresses and the SSH key file.

The environment can be changed by adding `--extra-vars "env=prod"`.
By default, the environment is set to dev.

## install_server
This playbook is used to install the web server, copy their configurations and the static files.

The command in below will install the server _Apache_ on the prod environment.

```shell
ansible-playbook -i hosts.ini playbooks/install_server/main.yml --extra-vars "server={{apache}} env=prod" --forks 15
```

The available servers can be viewed, added and deleted in [servers-default.yml](playbooks/install_server/vars/servers-default.yml).

## ab
This playbook installs _Apache Benchmark_, runs the benchmark, process the data to a CSV file and copies
the files to the local machine.

The command in below runs in the dev environment, stops all the web servers, starts Nginx and runs the 
benchmark against it with a 10000 number of requests, 200 concurrency to the static file of 
20KB size. It also skips the cloning of the repository containing the Python script to process the 
_ab_ data to a CSV file.

```shell
ansible-playbook -i hosts.ini playbooks/ab/main.yml --extra-vars "server={{nginx}} n=10000 c=200 path=/static-20k.html skip_clone=True" --forks 15
```

At the end of the run, in _`playbooks/ab/out/{ip}/out.csv`_ there will be the csv file containing
the collected data. Additional hardware data is added to the CSV file,
in [extra_data](playbooks/ab/vars/servers-default.yml) can be found which data is added.

###### CSV file example

| server          | host      | port | path             | doc_len | conc | time  | r_comp | r_fail | rps      | tpr    | tpr_all | 50 | 66 | 75 | 80 | 90 | 95 | 98 | 99 | 100 | iso8601              | os           | arc    | memtotal_mb | disk         | pcores | vcpus |
|-----------------|-----------|------|------------------|---------|------|-------|--------|--------|----------|--------|---------|----|----|----|----|----|----|----|----|-----|----------------------|--------------|--------|-------------|--------------|--------|-------|
| Apache/2.4.52   | 127.0.0.1 | 80   | /static-20k.html | 20480   | 200  | 3.077 | 50000  | 0      | 16248.86 | 12.309 | 0.062   | 12 | 12 | 13 | 13 | 15 | 16 | 19 | 21 | 48  | 2023-12-08T11:00:38Z | Ubuntu_22.04 | x86_64 | 31334       | 362633863168 | 16     | 16    |
| nginx/1.18.0    | 127.0.0.1 | 80   | /static-20k.html | 20480   | 200  | 2.876 | 50000  | 0      | 17387.81 | 11.502 | 0.058   | 11 | 12 | 12 | 13 | 14 | 16 | 18 | 19 | 24  | 2023-12-08T11:07:50Z | Ubuntu_22.04 | x86_64 | 31334       | 362633863168 | 16     | 16    |
| lighttpd/1.4.63 | 127.0.0.1 | 80   | /static-20k.html | 20480   | 200  | 2.457 | 50000  | 0      | 20349.14 | 9.828  | 0.049   | 9  | 10 | 10 | 10 | 12 | 14 | 17 | 21 | 26  | 2023-12-08T11:13:44Z | Ubuntu_22.04 | x86_64 | 31334       | 362633863168 | 16     | 16    |
| Caddy           | 127.0.0.1 | 80   | /static-20k.html | 20480   | 200  | 2.863 | 50000  | 0      | 17463.82 | 11.452 | 0.057   | 11 | 11 | 12 | 13 | 14 | 16 | 17 | 18 | 72  | 2023-12-08T11:21:53Z | Ubuntu_22.04 | x86_64 | 31334       | 362633863168 | 16     | 16    |
| LiteSpeed       | 127.0.0.1 | 80   | /static-20k.html | 20480   | 200  | 2.537 | 50000  | 0      | 19708.62 | 10.148 | 0.051   | 9  | 10 | 11 | 11 | 14 | 16 | 18 | 19 | 26  | 2023-12-08T11:28:53Z | Ubuntu_22.04 | x86_64 | 31334       | 362633863168 | 16     | 16    |


# R
[plots.R](plots.R) contains the script to generate the plots.
The documentation for it is added as comments in the code.
