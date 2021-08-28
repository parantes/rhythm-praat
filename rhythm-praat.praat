# coupled_oscillators.praat
#
# author: Pablo Arantes
# created: mar 08 2010
# modified: jun 13 2010
#
# Praat implementation of Barbosa's coupled-oscillator model of
# rhythm production. Original Matlab source code can be found in
# Barbosa (2006, pp. 480-485).
#
# Variables
# ---------
# alpha: entrainment rate
# beta: decay rate
# T0: syllable oscillator resting period
# w0: relative coupling strength
# catalexis: remaining syllables in the last stress group

form Model parameters
	real Alpha 0.4
	real Beta 1.1
	real T0_(in_sec) 0.165
 	real W0 0.78
	natural Number_of_stress_groups 2
	sentence Units_per_stress_group 4 4
	sentence Phrase_stress_amplitudes 1 0.5
	integer Catalexis 1
	choice Resetting_method: 1
		button Fixed length
		button Variable length
	boolean Plot 1
	boolean Save_simulation 0
	sentence Folder C:\pablo\
	sentence File sim1.txt
endform

# GUI variable name shortening
groups = number_of_stress_groups
units$ = units_per_stress_group$
amplitudes$ = phrase_stress_amplitudes$

# Value separator in input strings (units$ and amplitudes$)
sep$ = " "

call string_to_table groups catalexis "'units$'" units
units = string_to_table.table_id

call string_to_table groups catalexis "'amplitudes$'" amplitudes
amplitudes = string_to_table.table_id

# Sum of all units in the stress groups
call sum_rows units
rows = sum_rows.sum

# Synchronization function values Table
sync = Create Table with column names... sync rows value

# Simulated durations Table
dur = Create Table with column names... dur rows value

# Computes sync function
call sync groups catalexis units sync

# Computes syllable oscillator period entrainment function
call entrainment groups catalexis units amplitudes sync dur resetting_method

# Draw simulated V-V unit duration plot if selected
if plot = 1
	call draw dur 1
endif

# Save simulated duration values in file if selected
if save_simulation = 1
	call save dur rows "'folder$'" 'file$'
endif

select dur
Set column label (index)... 1 duration
List... 0

select units
plus amplitudes
plus sync
Remove

# ----------------------------------------
# PROCEDURES
# ----------------------------------------

procedure count_char .char$ .str$
# Count occurences of a character in a string
#
# Variables
# ---------
# str$ : string where the occurences will be counted
# sep$ : value separator (blank space)
# count: frequency of occurence of char$

	.count = 0
	.char = index(.str$, .char$)
	while .char > 0
		.count += 1
		.len = length(.str$)
		.str$ = mid$(.str$, .char+1, .len-.char)
		.char = index(.str$, .char$)
	endwhile
endproc

procedure check_string .groups .sep$ .str$
# Check if data input strings are well-formed.
#
# Variables
# ---------
# str$ : string to be checked
# sep$ : value separator (blank space)

	# Number of values in string (space-separated numbers) should
	# coincide with the number specified in the 'groups' variable.
	# There should be no extra spaces between values and no 
	# trailing or leading spaces.
	call count_char "'.sep$'" '.str$'
	.control = count_char.count + 1
	if .control <> .groups
		exit ERRROR. Possible causes:'newline$'1. The string"'.str$'" does not have '.groups' values.'newline$'2. There might be extra spaces in the string.
	endif
endproc


procedure string_to_table .rows .cat .str$ .table_name$
# Parse space-separated value strings and append values in a Table object
#
# Variables
# ---------
# rows: user-supplied value ('groups' GUI variable)
# cat: remaining units after last stress group ('catalexis' GUI variable)
# str$: input string to be parsed
# table_name$: Table object name
# table_id: Table object numerical ID for later use

	# Check string well-formedness before proceeding
	call check_string .rows "'sep$'" '.str$'

	.table_id = Create Table with column names... '.table_name$' .rows value
	.sep = index (.str$, sep$)
	.row = 1
	while (.sep > 0)
		.len = length (.str$)
		.value$ = left$ (.str$, .sep-1)
		.str$ = right$ (.str$, .len-.sep)
		select .table_id
		Set numeric value... .row value '.value$'
		.row += 1
		.sep = index (.str$, sep$)
	endwhile
	select .table_id
	Set numeric value... .rows value '.str$'

	# Catalexis
	if .cat > 0
		Append row
		if .table_name$ = "units"
			Set numeric value... .rows+1 value .cat
		else
			.value = Get value... .rows value
			Set numeric value... .rows+1 value .value
		endif
	endif
endproc

procedure sum_rows .table_id
# Sums the numerical entries of a Table column
#
# Variables
# ---------
# table_id: numerical id of Table object
# rows: number of Table rows

	.sum = 0
	select .table_id
	.rows = Get number of rows
	for .row to .rows
		value = Get value... .row value
		.sum += value
	endfor
endproc

procedure sync .groups .cat .units_table .sync_table
# Computes sync function for each unit in the stress groups
#
# Variables
# ---------
# groups: number of stress groups to be simulated ('groups' GUI variable)
# cat: remaining units after last stress group ('catalexis' GUI variable)
# units_table: units Table numerical ID
# sync_table: sync Table numerical ID

	# sync Table total rows
	select .sync_table
	.s_rows = Get number of rows
	# Row counter for sync Table
	.s_row = 1

	if .cat > 0
		.cat = 1
	endif

	for .group to (.groups + .cat)
		select .units_table
		.units = Get value... .group value
		for .unit to .units
			if .group <= .groups
				# Sync function
				# 1st unit
				if .unit = 1
					.s = w0 * exp(-.units + 2)
				# last unit
				elsif .unit = .units
					.s = w0 * exp(-5.81 + (0.016 * t0 * 1000))
				# general case
				else
					select .sync_table
					.s_previous = Get value... .s_row-1 value
					# units are counted beggining with 0, not 1
					# that's the reason to '.units-1' 
					.s = (1 - w0) * .s_previous + w0 * exp(-.units + (.unit-1) + 2)
				endif
				select .sync_table
				Set numeric value... .s_row value '.s:4'
				.s_row += 1
			else
				# sync values for units in catalexis are equal to
				# the last computed sync value
				select .sync_table
				.s = Get value... .s_row-1 value
				Set numeric value... .s_row value .s
				.s_row += 1
			endif
		endfor
	endfor
endproc

procedure entrainment .groups .cat .units_table .amp_table .sync_table .dur_table .resetting_method
# Computes entrained syllable oscillator abstract period duration
#
# Variables
# ---------
# groups: number of stress groups to be simulated ('groups' GUI variable)
# cat: remaining units after last stress group ('catalexis' GUI variable)
# units_table: units per stress group Table numerical ID
# amp_table: phrase stress amplitude Table numerical ID
# sync_table: sync Table numerical ID
# dur_table: simulated durations Table numerical ID
# .resetting_method: resetting method chosen by the user

	# dur Table total rows
	select .dur_table
	.s_rows = Get number of rows
	# Row counter for dur Table
	.dur_row = 1

	if .cat > 0
		.cat = 1
	endif

	for .group to (.groups + .cat)
		select .units_table
		.units = Get value... .group value

		# Resetting length
		if .resetting_method = 1
			.length = 2
		elsif .resetting_method = 2
			.length = round(0.7*.units)
		endif

		for .unit from 1 to .units
			# Utterance-initial unit is special
			if (.dur_row = 1)
				if w0 = 0
					.dur_previous = t0
				else
					.dur_previous = 7*t0*t0
				endif
			else
				select .dur_table
				.dur_previous = Get value... .dur_row-1 value
			endif
			if .group = 1
				# Initial group period resetting not modulated
				# by previous phrase stress amplitude
				.reset = - beta * (.dur_previous - t0) * 1
			else
				select .amp_table
				.amp_previous = Get value... .group-1 value
				.reset = - beta * (.dur_previous - t0) * .amp_previous
			endif
			select .sync_table
			.s = Get value... .dur_row value
			select .amp_table
			.amp = Get value... .group value
			# Period entrainment computation
			.deltaT = (alpha * .dur_previous * .s * .amp)
			if .group <= .groups
				if (.unit <= .length)
					.deltaT = .deltaT + .reset
				endif
			endif
			.dur = .dur_previous + .deltaT
			# Append .dur value to sync Table
			select .dur_table
			Set numeric value... .dur_row value '.dur:4'
			.dur_row += 1
		endfor
	endfor
endproc

procedure sum_cols .dur_table .col
# Gets the sum of a Table
	select .dur_table
	.rows = Get number of rows
	.col$ = Get column label... .col
	.sum = 0
	for .row to .rows
		.value = Get value... .row '.col$'
		.sum += .value
	endfor
endproc

procedure draw .dur_table .col
# Draw simulated V-to-V simulated durations
	.x_margin = 0.03
	.y_margin = 0.015
	# Marks every .y_marks milisseconds
	.y_marks = 25
	select .dur_table
	.col$ = Get column label... .col
	.y_max = Get maximum... '.col$'
	.y_min = Get minimum... '.col$'
	call sum_cols .dur_table .col
	.x_max = sum_cols.sum
	.rows = Get number of rows

	Erase all
	Axes... (0-.x_margin) (.x_max+.x_margin) (.y_min-.y_margin) (.y_max+.y_margin)
	.time = 0
	clearinfo
	for .row to .rows
		select .dur_table
		.dur = Get value... .row value
		.time += .dur
		Paint circle... black .time .dur 0.008
		if .row = 1
			Grey
			Draw line... 0 .dur .dur .dur
			Black
		else
			Grey
			Draw line... .time_p .dur_p .time .dur
			Black
			if .row = .rows
				One mark bottom... .time no yes no '.time:2'
			endif
		endif
		.dur_p = .dur
		.time_p = .time
	endfor
	Marks left every... 0.001 .y_marks yes yes no
	One mark bottom... 0 yes yes no
	Text left... yes abstract duration (ms)
	Text bottom... yes time (s)
	Draw inner box
endproc

procedure save .dur_table .rows .folder$ .file$
# Save simulated duration values
#
# Variables
# ---------
# dur_table: simulated values Table ID
# rows: simulated values Table rows

	.file$ = .folder$ + .file$
	filedelete '.file$'
	for .row to .rows
		select .dur_table
		.value = Get value... .row value
		fileappend '.file$' '.value''newline$'
	endfor
endproc
