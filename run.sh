#!/usr/bin/env bash
# set -e

uart_h2c=/tmp/repl/myid/uart.h2c
uart_c2h=/tmp/repl/myid/uart.c2h

# Build controller image
pushd $(west topdir)/lhost/apps/hci_sim
west build -b nrf52_bsim
popd

hci_uart="$(west topdir)/lhost/apps/hci_sim/build/zephyr/zephyr.exe"

mkdir -p $(dirname ${uart_h2c})
mkfifo ${uart_h2c}
mkfifo ${uart_c2h}

# Cleanup all existing sims
~/sdk/bsim/components/common/stop_bsim.sh

# This talks to the REPL
$hci_uart \
    -s=myid -d=1 -RealEncryption=0 -rs=70 \
    -fifo_0_rx=${uart_h2c} \
    -fifo_0_tx=${uart_c2h} &

# TODO: start peer device

# Force sim to (kinda) real-time
pushd "${BSIM_COMPONENTS_PATH}/device_handbrake"
./bs_device_handbrake -s=myid -d=0 -r=10 &

# Start the PHY
pushd "${BSIM_OUT_PATH}/bin"
./bs_2G4_phy_v1 -s=myid -D=2
