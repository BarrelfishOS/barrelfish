rm -rf build
mkdir build && cd build
../hake/hake.sh -s ../ -a x86_64
make X86_64_Full -j 8
sudo make qemu_x86_64