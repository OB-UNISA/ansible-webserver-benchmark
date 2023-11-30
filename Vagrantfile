# -*- mode: ruby -*-
# vi: set ft=ruby :

Vagrant.configure("2") do |config|
  config.vm.box = "hashicorp/bionic64"

  config.ssh.username = "vagrant"
  config.ssh.password = "vagrant"

  config.vm.synced_folder ".", "/vagrant", disable: true

  # App server 1
  config.vm.define "s1" do |app|
    app.vm.hostname = "s1.test"
    app.vm.network :private_network, ip: "192.168.60.4"
  end

  # App server 2
  config.vm.define "s2" do |app|
    app.vm.hostname = "s2.test"
    app.vm.network :private_network, ip: "192.168.60.5"
  end

  # DB Server
  config.vm.define "s3" do |app|
    app.vm.hostname = "s3.test"
    app.vm.network :private_network, ip: "192.168.60.6"
  end

end
