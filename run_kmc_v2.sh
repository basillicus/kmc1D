#!/bin/bash

# Given a set of initial paremeters, runs a number of KMC simulations varying
# the parameters acording to the deltaXY's values.
#
# ---  Tune here the initial parameters ---
PES="0" #> Sets the kind_of_PES parameter 0: No intermediates; 1: 1 intermediate
#seed="-1"
# e1="0.339195356311"; e2="0.315975255624"; e3="0.1473708746489"; e4="0.170590975336"  #> Set the initial energy barriers
e1=".19"; e2=".19";   #> Set the initial energy barriers
deltaE1="0.25"; deltaE2="0.25"  #> Set the variation of the barriers
deltaE3="-0.01"  #> Set the variation of the intermediate
#
# pf1="13.9217"; pf2="36.0214"; pf3="10.5505"; pf4="4.0776" #> Set the initial prefactors
pf1="10"; pf2="10"; pf3="10"; pf4="10" #> Set the initial prefactors
deltaPf1=0; deltaPf2=0; deltaPf3=0; deltaPf4=0 #> Set the variation of the prefactors

temperature=300
deltaT=-50

alpha=".000"
delta_alpha="0.050"
oscilatory=".true."
freq_field="10000"
#------------------------------------------
#
#
#------------------------------------------
nruns=2  #> Number of KMC runs with the same set of values (different seeds) to generate statistics
nvariations_1=10 #> How many times the parameter 1 will be changed by deltaX (alpha)
nvariations_2=4 #> How many times the parameter 2 will be changed by deltaX (energies)

nkmcsteps=5000000
dt=500
time_interval="0.001" #> In miliseconds
idx=0
# -----------------------------------------

# -----------------------------------------
# Storing the initial parameters for later on
# No need to be modified
e1_ini=$e1; e2_ini=$e2; e3_ini=$e3; e4_ini=$e4
pf1_ini=$pf1; pf2_ini=$pf2; pf3_ini=$pf3; pf4_ini=$pf4
temperature_ini=$temperature
alpha_ini=$alpha
# -----------------------------------------

echo "# Set of KMC simulations" > run_kmc.info
echo "# N of simulations/set of parameters: $nruns" >> run_kmc.info
echo "# N of variations of parameters: $nvariations" >> run_kmc.info

function run_simulations {
    for i in $(seq 1 $nvariations_1); do
        dir="alpha_$alpha"
        mkdir $dir
        echo "Running in $dir ..."
        cd $dir
        for i in $(seq 1 $nvariations_2); do
            update_input
            cat > input.dat << EOF
$input
EOF
            echo "   Energies $e1 $e2 ..."
            for n in $(seq 1 $nruns); do
                kmc1d > /dev/null
                mv kmc_time.out alpha_${alpha}_T_${temperature}_barrier_${e1}_sim_${n}.out
            done
            update_parameters $2  # Updated the inner loop parameters
        done
        reset_parameters $2    # Reset the parameters of the first loop
        update_parameters $1   # Update the outter loop parameter
        cd ..
    done
}

function update_input {
input="
printing 1
$seed
kind_of_PES $PES
constant_time_step $dt
barriers $e1 $e2 $e3 $e4 
prefactors  $pf1  $pf2 $pf3 $pf4
temperature $temperature
alpha $alpha
oscilatory_field $oscilatory
freq_field $freq_field
number_of_kmc_steps  $nkmcsteps
time_interval $time_interval
"
}

function update_parameters {
    if [ $1 == "temperature" ]; then 
        temperature=$(echo "$temperature + $deltaT " | bc )
    elif [ $1 == "energies" ]; then
            e1=$(echo "$e1 + $deltaE1"| bc)
            e2=$(echo "$e2 + $deltaE1"| bc)
        echo "Energies: " $e1   $e2
    elif [ $1 == "alpha" ]; then
        alpha=$(echo "$alpha + $delta_alpha" | bc )
    fi
}

function reset_parameters {
    if [ $1 == "energies" ]; then 
        e1=$e1_ini; e2=$e2_ini; e3=$e3_ini; e4=$e4_ini  #> Re-set to the initial energy barriers
    elif [ $1 == "temperature" ]; then
        temperature=$temperature_ini
    fi
}

# Call the function run_simulations and pass the parameters you want to update
# in the outter loop as $1 and in the inner loop as $2. If you only want to
# update the inner loop you can call the function as: 
# run_simulations nothing temperature  
run_simulations alpha energies


