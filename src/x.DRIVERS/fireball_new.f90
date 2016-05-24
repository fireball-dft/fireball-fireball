! copyright info:
!
!                             @Copyright 2012
!                           Fireball Committee
! West Virginia University - James P. Lewis, Chair
! Arizona State University - Otto F. Sankey
! Universidad Autonoma de Madrid - Jose Ortega
! Academy of Sciences of the Czech Republic - Pavel Jelinek

! Previous and/or current contributors:
! Auburn University - Jian Jun Dong
! Caltech - Brandon Keith
! Dublin Institute of Technology - Barry Haycock
! Pacific Northwest National Laboratory - Kurt Glaesemann
! University of Texas at Austin - Alex Demkov
! Ohio University - Dave Drabold
! Washington University - Pete Fedders
! West Virginia University - Ning Ma and Hao Wang
! also Gary Adams, Juergen Frisch, John Tomfohr, Kevin Schmidt,
!      and Spencer Shellman

!
! RESTRICTED RIGHTS LEGEND
! Use, duplication, or disclosure of this software and its documentation
! by the Government is subject to restrictions as set forth in subdivision
! { (b) (3) (ii) } of the Rights in Technical Data and Computer Software
! clause at 52.227-7013.

! Program Description
! ===========================================================================
!> This is the main driver for the LIGHTNING version of FIREBALL.
! ==========================================================================
! Code written by:
!> @author Ning Ma
!! @author James P. Lewis
! Box 6315, 209 Hodges Hall
! Department of Physics
! West Virginia University
! Morgantown, WV 26506-6315
!
! (304) 293-3422 x1409 (office)
! (304) 293-5732 (FAX)
! ===========================================================================
!
! Program Declaration
! ===========================================================================
        program fireball_MDET

! /SYSTEM
        use M_species
        use M_configuraciones
        use M_neighbors
        use M_neighbors_PP
        use M_atom_functions

! /FDATA
        use M_Fdata_1c
        use M_Fdata_2c
        use M_Fdata_3c
! /ASSEMBLERS
        use M_assemble_2c
        use M_assemble_3c
        use M_assemble_ewald
        use M_assemble_usr
        use M_assemble_vxc
        use M_assemble_PP_2c
        use M_assemble_PP_3c

! /DASSEMBLERS
        use M_Dassemble_2c
        use M_Dassemble_PP_2c
        use M_Dassemble_3c
        use M_build_forces
        use M_Dassemble_vxc
        use M_Dassemble_usr
! /MD
        use M_dynamics

! /SOLVESH
        use M_kspace
        use M_density_matrix

! /SCF
        use M_charges

        implicit none

! Argument Declaration and Description
! ===========================================================================
! None

! Parameters and Data Declaration
! ===========================================================================
! None

! Variable Declaration and Description
! ===========================================================================
        integer iatom                        !< counter over atoms
        integer in1                          !< defines species number
        integer iscf_iteration
        integer istructure, iseparate
        integer itime_step

        real sigma
        real time
        real vari                            !to chekc varaibles Arturo
        character (len = 25) :: slogfile

        ! for reading in wavefunctions
        integer iten, ione, itwo, ithree !< character array places
        integer ipoint                   !< counter over character point
        integer ispecies                !< loop over species
        integer issh                    !< loop over shells
        integer nZ                       !< atomic number
        integer cont
        real rcbohr                      !< cutoff in Bohr radii

        character (len = 11) buffer     !< buffer for generating wavefunction
        character (len = 3) rcchar
        character (len = 1), dimension (0:9) :: zchar

! --------------------------------------------------------------------------
! Timer (Intel Fortran)
! --------------------------------------------------------------------------
        real time_begin
        real time_end
        real timei, timef

! Energies
        real atomic_energy                           ! total atomic energy
        real ebs                                     ! band-structure energy
        real uii_uee, uxcdcc                         ! short-range energies
        real etot                                    ! total energy
        real etot_per_atom                           ! total energy per atom

! Temporary for debugging forces
        real ebs_previous
        real etot_previous
! Allocate Arrays
! ===========================================================================
! None

! Procedure
! ===========================================================================
! Temporary assignment
        ebs_previous = 0.0d0
        etot_previous = 0.0d0
! ===========================================================================
! ---------------------------------------------------------------------------
!                             W E L C O M E !
! ---------------------------------------------------------------------------
! ===========================================================================
        call cpu_time (time_begin)
        ilogfile = 21
        iseparate = 1
        open (unit = ilogfile, file = 'output.log', status = 'replace')
        call welcome

! ===========================================================================
! ---------------------------------------------------------------------------
!             R E A D   I N   S Y S T E M   I N F O R M A T I O N
! ---------------------------------------------------------------------------
! ===========================================================================
        call read_Fdata_location
        call read_info
        call read_Fdata_1c
        call read_Fdata_2c
        call read_Fdata_3c

! Read in the wavefunctions
        call read_wavefunctions

! Read parameters from structures.inp file
        write (ilogfile,*)
        write (ilogfile,*) ' Reading parameters from the structure input. '
        call read_parameters

        write (ilogfile, *)
        open (unit = 1, file = 'structures.inp', status = 'old')
        read (1, *) nstructures
        if (nstructures .gt. 999) then
          stop ' Cannot calculate more than 999 structures! '
        end if
        write (ilogfile, *) ' Number of structure calculating = ', nstructures
        allocate (structures (nstructures))

! Loop over all structures
! This loop can be made parallel if each subroutine in lightning
! is modified to take s as a parameter, rather than reference s directly
!$omp parallel do private (s, slogfile, sigma, iscf_iteration, timei, timef) &
!$omp             private (ebs, , uxcdcc, etot)
        do istructure = 1, nstructures
          s => structures(istructure)
          read (1, *) s%basisfile
          if (iseparate .eq. 1) then
            s%logfile = istructure + 1000
            s%inpfile = istructure + 2000
            slogfile = s%basisfile(:len(trim(s%basisfile)) - 4)
            slogfile = trim(slogfile)//'.log'
            open (unit = s%logfile, file = slogfile, status = 'replace')
          end if
          write (ilogfile,*)
          write (ilogfile, 100) slogfile
          write (s%logfile, *) ' Structure = ', istructure

          ! Read in the coordinates and parameters
          call read_positions (s)

          ! Set the charges for istructure
          call read_charges (s)

          call set_constraints (s)

! Molecular-dynamics loop
! ---------------------------------------------------------------------------
! ===========================================================================
! FIXME - We set nstepi and nstepf here!
          nstepi = 1
          !nstepf = 1
          time = 0.0d0
          do itime_step = nstepi, nstepf

! ===========================================================================
! ---------------------------------------------------------------------------
!           N E I G H B O R S   S Y S T E M   I N F O R M A T I O N
! ---------------------------------------------------------------------------
! ===========================================================================
            print *, itime_step, 'itime_step'
            !if (itime_step .eq. nstepi) then
            call driver_neighbors (s)
            call driver_neighbors_PP (s)
            !end if
! ===========================================================================
! ---------------------------------------------------------------------------
!              A S S E M B L E   T H E   H A M I L T O N I A N
! ---------------------------------------------------------------------------
! ===========================================================================
! Assemble the Hamiltonian matrix:
            write (s%logfile, *)
            write (s%logfile, *) ' Calling two-center non-charge dependent assemblers. '
            call assemble_S (s)
            call assemble_T (s)
!           call assemble_dipole_z (s)
            call assemble_svnl (s)
            call assemble_vnl_2c (s)

            write (s%logfile,*) ' Calling three-center non-charge dependent assemblers. '
            call assemble_vnl_3c (s)

! Put scf loop here
            cont= 0
            sigma = 999.0d0
            iscf_iteration = 1
            do while (sigma .gt. scf_tolerance_set .and.                  &
      &                 iscf_iteration .le. max_scf_iterations_set)
              write (s%logfile, *)
              write (s%logfile, *) ' Begin scf step = ', iscf_iteration
              write (s%logfile, *) ' Calling two-center charge dependent assemblers. '
              call assemble_vna_2c (s)
!             call assemble_ewaldsr (s)
!             call assemble_ewaldlr (s)

              write (s%logfile, *) ' Calling three-center charge dependent assemblers. '
              call assemble_vna_3c (s)
              call assemble_vxc (s)

! ===========================================================================
! ---------------------------------------------------------------------------
!                         D I A G O N A L I Z E
! ---------------------------------------------------------------------------
! ===========================================================================
! Calculating the overlap matrix in K space
              write (s%logfile, *) ' Calling kspace: '
              call cpu_time(timei)
              call driver_kspace (s, iscf_iteration)
              call cpu_time(timef)
              write (s%logfile, *)
              write (s%logfile, *) ' kspace time: ', timef - timei
              call density_matrix (s)
              if (iwriteout_density .eq. 1) call writeout_density (s)

              call calculate_charges (s)
              if (iwriteout_charges .eq. 1) call writeout_charges (s)
              call Qmixer (s, iscf_iteration, sigma)

              ! Assemble the derivative blocks needed for forces
              call initialize_forces (s)
              call Dassemble_S (s)
              call Dassemble_T (s)
              call cape_matrix (s) !arturo
              call Dassemble_vna_2c (s)
              call densityPP_matrix (s)
              call Dassemble_svnl (s)
              call Dassemble_vnl_2c (s)
              call Dassemble_vna_3c (s)
              call Dassemble_vxc (s)
              call Dassemble_vxc_3c (s)
              call Dassemble_uee (s)
              call Dassemble_uxc (s)
              call build_forces (s)


! ===========================================================================
! ---------------------------------------------------------------------------
!                       T O T A L   E N E R G I E S
! ---------------------------------------------------------------------------
! ===========================================================================
              call calculate_ebs (s, ebs)
              print *, ebs, 'ebs'
              call assemble_uee (s, uii_uee)
              uii_uee = 0.0d0
              call assemble_uxc (s, uxcdcc)
              uxcdcc = 0.0d0

              ! Writing out the energy pieces
              write (s%logfile, *)
              write (s%logfile, *) ' ---------- T H E  T O T A L  E N E R G Y ----------- '
              write (s%logfile, *)
              write (s%logfile, 502) ebs
              write (s%logfile, 503) uii_uee
              write (s%logfile, 505) uxcdcc

              ! Evaluate total energy
              etot = ebs + uii_uee + uxcdcc
              write (s%logfile, 507) etot

              ! Total energy per atom
              etot_per_atom = etot/s%natoms
              write (s%logfile, 508) etot_per_atom

              ! Cohesive Energy
              atomic_energy = 0.0d0
              do iatom = 1, s%natoms
                in1 = s%atom(iatom)%imass
                atomic_energy = atomic_energy + species(in1)%atomicE
              end do
              write (s%logfile, 509) atomic_energy
              write (s%logfile, 510) etot - atomic_energy
              write (s%logfile, *)
              write (s%logfile, 511) (etot - atomic_energy)/s%natoms
              write (s%logfile, *) ' ----------------------------------------------------- '

! End scf loop

              if (sigma .gt. 0.0d0) then
                iscf_iteration = iscf_iteration + 1
              else
                exit
              end if
            end do
            if (istructure .ne. 1) then
                !write (31,*) istructure, -(ebs - ebs_previous)/0.01d0
                write (31,*) istructure, ebs
                !write (32,*) istructure, sqrt(((etot - etot_previous)/0.01d0)**2)
                write (32,*) istructure, ((etot - etot_previous)/0.005d0)
            end if

            write (47,*) istructure, etot
            write (46,*) istructure, uxcdcc
            write (43,*) istructure, uii_uee
            !write (70,*) istructure, sqrt( (s%forces(1)%ftot(3))**2 + (s%forces(1)%ftot(2))**2 + (s%forces(1)%ftot(1))**2)
            !write (71,*) istructure, sqrt( (s%forces(2)%ftot(3))**2 + (s%forces(2)%ftot(2))**2 + (s%forces(2)%ftot(1))**2)
            !write (72,*) istructure, sqrt( (s%forces(3)%ftot(3))**2 + (s%forces(3)%ftot(2))**2 + (s%forces(3)%ftot(1))**2)
            !write (73,*) istructure, sqrt((s%forces(3)%ftot(1)+ s%forces(2)%ftot(1)+ s%forces(1)%ftot(1))**2 + &
            !&                             (s%forces(3)%ftot(2)+ s%forces(2)%ftot(2)+ s%forces(1)%ftot(2))**2 + &
            !&                             (s%forces(3)%ftot(3)+ s%forces(2)%ftot(3)+ s%forces(1)%ftot(3))**2)

            !write (70,*) istructure, sqrt( (s%forces(1)%ftot(3))**2 + (s%forces(1)%ftot(2))**2 + (s%forces(1)%ftot(1))**2)
            !write (71,*) istructure, sqrt( (s%forces(2)%ftot(3))**2 + (s%forces(2)%ftot(2))**2 + (s%forces(2)%ftot(1))**2)

            write (70,*) istructure, s%forces(1)%ftot(3)
            write (71,*) istructure, s%forces(2)%ftot(3)

            write (73,*) istructure, sqrt(( s%forces(2)%ftot(1)+ s%forces(1)%ftot(1))**2 + &
            &                             ( s%forces(2)%ftot(2)+ s%forces(1)%ftot(2))**2 + &
            &                             ( s%forces(2)%ftot(3)+ s%forces(1)%ftot(3))**2)
            ebs_previous = ebs
            etot_previous = etot
            if (iwriteout_xyz .eq. 1) then
              call writeout_xyz (s, ebs, uii_uee, uxcdcc)
            end if



          end do ! end molecular dynamics loop

! Destroy Hamiltonian matrix elements storage
          call destroy_assemble_2c (s)
          call destroy_assemble_PP_2c (s)
          call destroy_assemble_vxc_McWEDA (s)

! Calculate the absorption spectrum.
          if (iwriteout_abs .eq. 1) call absorption (s)

! Destroy final arrays
          call destroy_kspace (s)
          call destroy_denmat (s)
          call destroy_charges (s)

          call destroy_neighbors (s)
          call destroy_neighbors_PP (s)

          ! destroy neighbors last
          deallocate (s%xl) ! where to put this?

! Calculate the electronic density of states.
! We do this after destorying some arrays so that we can optimize the
! memory usage.
          if (iwriteout_dos .eq. 1) call dos (s)

          if (iseparate .eq. 1) close (s%logfile)
        end do ! end loop over all structures
        close (1)

! ===========================================================================
! ---------------------------------------------------------------------------
!               D E S T R O Y   A R R A Y S - F I N A L I Z E
! ---------------------------------------------------------------------------
! ===========================================================================
! Destroy datafile storage
        call destroy_Fdata_1C
        call destroy_Fdata_2C
        call destroy_Fdata_3c

! Destroy SYSTEM information.
        call destroy_positions
        call destroy_species

        call cpu_time (time_end)

        write (ilogfile,*) ' FIREBALL RUNTIME : ', time_end-time_begin, '[sec] '
        write (*,*) ' FIREBALL RUNTIME : ', time_end-time_begin, '[sec] '
        close (ilogfile)

! Deallocate Arrays
! ===========================================================================
! None

! Format Statements
! ===========================================================================
100     format (2x, ' Working on structure - ', a25)

502     format (2x, '           ebs = ', f15.6)
503     format (2x, '     uii - uee = ', f15.6)
505     format (2x, '        uxcdcc = ', f15.6)
507     format (2x, '          ETOT = ', f15.6)
508     format (2x, '     Etot/atom = ', f15.6)
509     format (2x, ' Atomic Energy = ', f15.6)
510     format (2x, '     CohesiveE = ', f15.6)
511     format (2x, ' Cohesive Energy per atom  = ', f15.6)

! End Program
! ===========================================================================
        stop
        end program fireball_MDET
