#!/bin/bash

# Given a set of initial paremeters, runs a number of KMC simulations varying
# the parameters acording to the deltaXY's values.
#
# ---  Tune here the initial parameters ---
e1="0.339195356311"; e2="0.315975255624"; e3="0.1473708746489"; e4="0.170590975336"  #> Set the initial energy barriers
deltaE1="-0.000"; deltaE2="-0.000"  #> Set the variation of the barriers
deltaE3="-0.01"  #> Set the variation of the intermediate
#
pf1="13.9217"; pf2="36.0214"; pf3="10.5505"; pf4="4.0776" #> Set the initial prefactors
deltaPf1=0; deltaPf2=0; deltaPf3=0; deltaPf4=0 #> Set the variation of the prefactors

temperature=300
deltaT=0

alpha=0
delta_alpha="0.0000"
#------------------------------------------
#
#
#------------------------------------------
nruns=20  #> Number of KMC runs with the same set of values to generate statistics
nvariations=1 #> How many times the parameters will be changed by deltaX
# nkmcsteps=10000000
#nkmcsteps=100000000
nkmcsteps=100000000
time_interval=10 #> In miliseconds
idx=0
# -----------------------------------------

# -----------------------------------------
# Storing the initial parameters for later on
# No need to be modified
e1_ini=$e1; e2_ini=$e2; e3_ini=$e3; e4_ini=$e4
pf1_ini=$pf1; pf2_ini=$pf2; pf3_ini=$pf3; pf4_ini=$pf4
# -----------------------------------------

echo "# Set of KMC simulations" > run_kmc.info
echo "# N of simulations/set of parameters: $nruns" >> run_kmc.info
echo "# N of variations of parameters: $nvariations" >> run_kmc.info
echo "# Idx   Mean X(arb. units)  Mean Time (ps)   E1    E2   E3   E4   Pref1 Pref2 Pref3  Pref4  Alpha" >> run_kmc.info

function update_energies {
    # Modify the height of the first barrier
    if [ $1 == "1" ]; then 
        e1=$(echo "$e1 + $deltaE1"| bc)
        e4=$(echo "$e4 + $deltaE1"| bc)
    # Modify the height of the second barrier
    elif [ $1 == "2" ]; then 
        e2=$(echo "$e2 + $deltaE2"| bc)
        e3=$(echo "$e3 + $deltaE2"| bc)
    # Modify the height of both barriers
    elif [ $1 == "12" ]; then 
        e1=$(echo "$e1 + $deltaE1"| bc)
        e4=$(echo "$e4 + $deltaE1"| bc)
        e2=$(echo "$e2 + $deltaE2"| bc)
        e3=$(echo "$e3 + $deltaE2"| bc)
    # Modify the height of the intermediate
    elif [ $1 == "3" ]; then 
        e3=$(echo "$e3 + $deltaE3"| bc)
        e4=$(echo "$e4 + $deltaE3"| bc)
    fi
}

function update_prefactors {
    # Modify prefactor X by deltaPfX
    if [ $1 -eq 1 ]; then 
        pf1=$(echo "$pf1 + $deltaPf1"| bc)
    elif [ $1 -eq 2 ]; then 
        pf2=$(echo "$pf2 + $deltaPf2"| bc)
    elif [ $1 -eq 3 ]; then 
        pf3=$(echo "$pf3 + $deltaPf3"| bc)
    elif [ $1 -eq 4 ]; then 
        pf4=$(echo "$pf4 + $deltaPf4"| bc)
    elif [ $1 -eq 12 ]; then 
        pf1=$(echo "$pf1 + $deltaPf1"| bc)
        pf2=$(echo "$pf2 + $deltaPf2"| bc)
    elif [ $1 -eq 13 ]; then 
        pf1=$(echo "$pf1 + $deltaPf1"| bc)
        pf3=$(echo "$pf3 + $deltaPf3"| bc)
    elif [ $1 -eq 14 ]; then 
        pf1=$(echo "$pf1 + $deltaPf1"| bc)
        pf4=$(echo "$pf4 + $deltaPf4"| bc)
    elif [ $1 -eq 23 ]; then 
        pf2=$(echo "$pf2 + $deltaPf2"| bc)
        pf3=$(echo "$pf3 + $deltaPf3"| bc)
    elif [ $1 -eq 24 ]; then 
        pf2=$(echo "$pf2 + $deltaPf2"| bc)
        pf4=$(echo "$pf4 + $deltaPf4"| bc)
    elif [ $1 -eq 123 ]; then 
        pf1=$(echo "$pf1 + $deltaPf1"| bc)
        pf2=$(echo "$pf2 + $deltaPf2"| bc)
        pf3=$(echo "$pf3 + $deltaPf3"| bc)
    elif [ $1 -eq 124 ]; then 
        pf1=$(echo "$pf1 + $deltaPf1"| bc)
        pf2=$(echo "$pf2 + $deltaPf2"| bc)
        pf4=$(echo "$pf4 + $deltaPf4"| bc)
        pf3=$(echo "$pf2 * $pf4 / $pf1"| bc -l)
    elif [ $1 -eq 134 ]; then 
        pf1=$(echo "$pf1 + $deltaPf1"| bc)
        pf3=$(echo "$pf3 + $deltaPf3"| bc)
        pf4=$(echo "$pf4 + $deltaPf4"| bc)
        pf2=$(echo "$pf1 * $pf3 / $pf4"| bc -l)
    elif [ $1 -eq 234 ]; then 
        pf2=$(echo "$pf2 + $deltaPf2"| bc)
        pf3=$(echo "$pf3 + $deltaPf3"| bc)
        pf4=$(echo "$pf4 + $deltaPf4"| bc)
        pf1=$(echo "$pf2 * $pf4 / $pf3"| bc -l)
    elif [ $1 -eq 1234 ]; then 
        pf1=$(echo "$pf1 + $deltaPf1"| bc)
        pf2=$(echo "$pf2 + $deltaPf2"| bc)
        pf3=$(echo "$pf3 + $deltaPf3"| bc)
        pf4=$(echo "$pf4 + $deltaPf4"| bc)
    fi
}

function update_temperature {
    temperature=$(echo "$temperature + $deltaT" | bc )
}
function update_alpha {
    alpha=$(echo "$alpha + $delta_alpha" | bc )
}

function reset_energies {
    e1=$e1_ini; e2=$e2_ini; e3=$e3_ini; e4=$e4_ini           #> Re-set to the initial energy barriers
}
function reset_prefactors {
    pf1=$pf1_ini; pf2=$pf2_ini; pf3=$pf3_ini; pf4=$pf4_ini   #> Re-set to the initial prefactors
}

function plot_kmc {

    cat > tmp_plot_kmc.gp << EOF
set terminal postscript enhanced
set output "sim_${idx}_E_${e1}_${e2}_${e3}_${e4}_pr_${pf1}_${pf2}_${pf3}_${pf4}.eps"
set title "E1:${e1}; E2:${e2}; E3:${e3}; E4:${e4}\n P1:${pf1}, P2:${pf2}, P3:${pf3}, P4:${pf4}"
set xlabel "Time (ps)"
set ylabel "Distance (arb. units)"

plot 'kmc.out' u 1:2 w l t "T=$temperature"
EOF
gnuplot tmp_plot_kmc.gp 
rm tmp_plot_kmc.gp
 let idx=$idx+1
}


function run_alpha {
    # Sim: Corresponds to the kind of variation of the parameters 
    for i in $(seq 1 $nvariations); do
        mean_x=0; mean_t=0
        cat > input.dat << EOF
printing 1
barriers $e1 $e2 $e3 $e4 
prefactors  $pf1  $pf2 $pf3 $pf4
temperature $temperature
alpha $alpha
number_of_kmc_steps  $nkmcsteps
time_interval $time_interval
EOF
        for n in $(seq 1 $nruns); do
            kmc1d > /dev/null
            tmp_x=$(tail -n 1 kmc.out | awk '{print $2}')
            tmp_t=$(tail -n 1 kmc.out | awk '{print $1}')
#             mean_x=$(echo "$mean_x + $tmp_x" | bc)
#             mean_t=$(echo "$mean_t + $tmp_t" | bc)
            echo  "$idx  $tmp_x       $tmp_t           $e1 $e2 $e3 $e4 $pf1 $pf2 $pf3 $pf4   $alpha" >> run_kmc.info
            # plot_kmc 
            mv kmc_time.out alpha_${alpha}_${n}.out
        done
        # mean_x=$(echo "($mean_x)/$nruns" | bc)
        # mean_t=$(echo "($mean_t)/$nruns" | bc)
        # Plot the last simulation of the set of values
        # Update the alphas to a new set of values
        update_alpha 
    done
}
function run_energies {
    # Sim: Correspond to the kind of variation of the parameters 
    for sim in $1; do 
        for i in $(seq 1 $nvariations); do
            mean_x=0; mean_t=0
            cat > input.dat << EOF
printing 1
barriers $e1 $e2 $e3 $e4 
prefactors  $pf1  $pf2 $pf3 $pf4
temperature $temperature
number_of_kmc_steps  $nkmcsteps
EOF
            for n in $(seq 1 $nruns); do
                kmc1d > /dev/null
                tmp_x=$(tail -n 1 kmc.out | awk '{print $2}')
                tmp_t=$(tail -n 1 kmc.out | awk '{print $1}')
                mean_x=$(echo "$mean_x + $tmp_x" | bc)
                mean_t=$(echo "$mean_t + $tmp_t" | bc)
            done
            mean_x=$(echo "($mean_x)/$nruns" | bc)
            mean_t=$(echo "($mean_t)/$nruns" | bc)
            echo  "$idx  $mean_x       $mean_t           $e1 $e2 $e3 $e4     $pf1 $pf2 $pf3 $pf4" >> run_kmc.info
            # Plot the last simulation of the set of values
            plot_kmc 
            # Update the energies to a new set of values
            update_energies $sim
        done
        reset_energies
    done
}


function run_prefactors {

    # Sim: Correspond to the kind of variation of the parameters 
    for sim in $1; do 
        for i in $(seq 1 $nvariations); do
            mean_x=0; mean_t=0
            cat > input.dat << EOF
printing 1
barriers $e1 $e2 $e3 $e4 
prefactors  $pf1  $pf2 $pf3 $pf4
temperature $temperature
number_of_kmc_steps  $nkmcsteps
EOF
            for n in $(seq 1 $nruns); do
                kmc1d > /dev/null
                tmp_x=$(tail -n 1 kmc.out | awk '{print $2}')
                tmp_t=$(tail -n 1 kmc.out | awk '{print $1}')
                mean_x=$(echo "$mean_x + $tmp_x" | bc)
                mean_t=$(echo "$mean_t + $tmp_t" | bc)
                plot_kmc
            done
            mean_x=$(echo "($mean_x)/$nruns" | bc)
            mean_t=$(echo "($mean_t)/$nruns" | bc)
            echo  "$mean_x       $mean_t           $e1 $e2 $e3 $e4     $pf1 $pf2 $pf3 $pf4" >> run_kmc.info
            # Plot the last simulation of the set of values
            # plot_kmc 
            # Update the prefactors to a new set of values
            update_prefactors $sim
        done
        reset_prefactors
    done
}

function run_temperature {

    # Sim: Correspond to the kind of variation of the parameters 
    for i in $(seq 1 $nvariations); do
        mean_x=0; mean_t=0
        cat > input.dat << EOF
printing 1
barriers $e1 $e2 $e3 $e4 
prefactors  $pf1  $pf2 $pf3 $pf4
temperature $temperature
number_of_kmc_steps  $nkmcsteps
EOF
        for n in $(seq 1 $nruns); do
            kmc1d > /dev/null
            tmp_x=$(tail -n 1 kmc.out | awk '{print $2}')
            tmp_t=$(tail -n 1 kmc.out | awk '{print $1}')
            mean_x=$(echo "$mean_x + $tmp_x" | bc)
            mean_t=$(echo "$mean_t + $tmp_t" | bc)
        done
        mean_x=$(echo "($mean_x)/$nruns" | bc)
        mean_t=$(echo "($mean_t)/$nruns" | bc)
        echo  "$mean_x       $mean_t           $e1 $e2 $e3 $e4     $pf1 $pf2 $pf3 $pf4     $temperature" >> run_kmc.info
        # Plot the last simulation of the set of values
        plot_kmc 
        # Update the prefactors to a new set of values
        update_temperature 
    done
}

run_alpha 
# echo "# Varying prefactors" >> run_kcm.info
#run_prefactors "134"
# run_temperature
