# Dragon v5.1

This is the repository for the Dragon code.
Version5 is a 64-bit clean distribution of the reactor physics codes developed at Polytechnique Montréal.


## License

This software is distributed and copyrighted according to the [LICENSE](LICENSE) file.


## Documentation

For comprehensive documentation please refer to the [Merlin site](http://merlin.polymtl.ca/version5.htm).
Additionally, you can build the documentation yourself using Latex, see the subdirectory [doc](./doc) for more.


## Dependencies

In order to compile and run Dragon you must have a Fortran compiler (2003 or later standard) and Python3.

Optional dependencies are [OpenMP](https://www.openmp.org/resources/openmp-compilers-tools/) and [HDF5](https://www.hdfgroup.org/download-hdf5/).


## Cloning

To clone this repository from the command line you will need `git` installed. To obtain the code run:

```bash
git clone https://git.oecd-nea.org/dragon/5.1
```

Note that cloning via https is the simplest as it requires no authentication, if you have an NEA account and you've added your ssh keys to your account you can also clone via ssh:

```bash
git clone git@git.oecd-nea.org:dragon/5.1.git
```

### LFS

This repository includes some data files used for testing, some of which are quite substantial in size (some up to 100 MB). As a result, Git LFS (Large File Storage), is used to store any files larger than 10 MB. For most cases, users will not need these files unless they wish to run tests locally.

If you wish to obtain these large files, you require Git LFS installed on your machine (see [here](https://git-lfs.com/) for more details on this).


## Installation

Dragon is supported on Linux and Unix machines. For Windows users we recommend using the Windows Subsystem for Linux (WSL).

The installation is based on `make`, particularly `gmake` for Unix systems.

Please go to the [Merlin site](http://merlin.polymtl.ca/version5.htm) for comprehensive installation instructions.


## Run the code

A minimum set of cross section files can be found on the [Merlin site](http://merlin.polymtl.ca/version5.htm), for more extensive work download the required files from the NEA data repository [here](../libraries/).

**Please note that you must preserve the directory structure**. This can be achieved by doing the following:

If you want all data (note the disk space ~10GB) follow these steps:

```bash
mkdir dragon
cd dragon
git clone https://git.oecd-nea.org/dragon/5.1
git clone https://git.oecd-nea.org/dragon/libraries
```

For those with limited disk space, you will need to specify which data files you want.

For example:

```bash
mkdir dragon
cd dragon
git clone https://git.oecd-nea.org/dragon/5.1
GIT_LFS_SKIP_SMUDGE=1 git clone https://git.oecd-nea.org/dragon/libraries
cd libraries
# For JEFF 3.1.1 SHEM 361 use the following
git lfs pull -I l_endian/draglibJeff3p1p1SHEM361_v5p1.gz
```

## Docker

If you don't want to compile the code on your machine, we offer pre-built Docker images for you to use to bypass this, and you just require docker installed on your machine.

For Windows users you can use [Docker Desktop](https://docs.docker.com/desktop/setup/install/windows-install/) (if you don't have WSL).


### Get image

To get a docker images (Ubuntu 22.04) with Dragon/Donjon already setup and paths set, you may pull this image via:

```bash
docker pull docker.oecd-nea.org/dragon/5.1:v5.1.0-slim
```

### Running image

A template command is provided below and you only need to change the text in the `<>` sections, specific to your installation. Note that the paths on the right hand side of the colon (:) should be to your **host** machine, the paths on the right hand side do not need to be changed and match to paths inside the docker container.


```bash
docker run -v <my_path_to_libs>:/dragon/libraries:ro
           -v <my_path_to_inputs>:/dragon/5.1/Donjon/data/:ro
           -v <my_path_to_outputs>:/dragon/5.1/Donjon/Linux_x86_64
           docker.oecd-nea.org/dragon/5.1:v5.1.0-slim
           "-q <my_input_file.x2m>"
```

You therefore need 4 parameters to set:

1. `<my_path_to_libs>` should point to the directory containing your Dragon nuclear data library file (`*.gz`).

2. `<my_path_to_inputs>` should point to a directory on your host machine containing the input data files (`*.x2m` and other related files). This should be a different directory to number 1.

3. `<my_path_to_outputs>` should point to a directory on your host machine which will be used to keep the output files from each code run i.e (including the `*.result` file). This should be different to 1. and 2.

4. `<my_input_file.x2m>` should be the name of the input data file which should exist in directory number 2.


An example of this is given below:


```bash
docker run -v /home/user/dragon/libraries:/dragon/libraries:ro
           -v /home/user/dragon/inputs:/dragon/5.1/Donjon/data/:ro
           -v /home/user/dragon/outputs:/dragon/5.1/Donjon/Linux_x86_64
           docker.oecd-nea.org/dragon/5.1:v5.1.0-slim
           "-q VanDerGucht.x2m"
```


given the following directory contents:

```bash
$ls /home/user/dragon/libraries
README.md  ascii  hdf5  l_endian  pynjoy
```

```bash
$ls /home/user/dragon/inputs
VanDerGucht.access  VanDerGucht.x2m  VanDerGucht_proc
```

will give me the following outputs

```bash
$ls /home/user/dragon/outputs
VanDerGucht.result
```
