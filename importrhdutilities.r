load_file = function(filename) {
    # Start timing
    start = Sys.time()
        
    # Open file
    fid = file(filename, "rb")
    filesize = file.info(filename)$size
    
    # Read file header
    header = read_header(fid)
    
    # Output a summary of recorded data
    cat(paste("Found ", header$num_amplifier_channels, " amplifier channel", plural(header$num_amplifier_channels), "\n", sep = ""))
    cat(paste("Found ", header$num_aux_input_channels, " auxiliary input channel", plural(header$num_aux_input_channels), "\n", sep = ""))
    cat(paste("Found ", header$num_supply_voltage_channels, " supply voltage channel", plural(header$num_supply_voltage_channels), "\n", sep = ""))
    cat(paste("Found ", header$num_board_adc_channels, " board ADC channel", plural(header$num_board_adc_channels), "\n", sep = ""))
    cat(paste("Found ", header$num_board_dig_in_channels, " board digital input channel", plural(header$num_board_dig_in_channels), "\n", sep = ""))
    cat(paste("Found ", header$num_board_dig_out_channels, " board digital output channel", plural(header$num_board_dig_out_channels), "\n", sep = ""))
    cat(paste("Found ", header$num_temp_sensor_channels, " temperature sensor channel", plural(header$num_temp_sensor_channels), "\n\n", sep = ""))
    
    # Determine how many samples the data file contains
    bytes_per_block = get_bytes_per_data_block(header)
    
    # Calculate how many data blocks are present
    data_present = 0
    bytes_remaining = filesize - seek(fid)
    if (bytes_remaining > 0) {
        data_present = 1
    }
    
    if ((bytes_remaining %% bytes_per_block) != 0) {
        stop("Something is wrong with file size: should have a whole number of data blocks")
    }
    
    num_data_blocks = bytes_remaining / bytes_per_block
    
    # Calculate how many samples of each signal type are present
    num_amplifier_samples = header$num_samples_per_data_block * num_data_blocks
    num_aux_input_samples = (header$num_samples_per_data_block / 4) * num_data_blocks
    num_supply_voltage_samples = num_data_blocks
    num_board_adc_samples = header$num_samples_per_data_block * num_data_blocks
    num_board_dig_in_samples = header$num_samples_per_data_block * num_data_blocks
    num_board_dig_out_samples = header$num_samples_per_data_block * num_data_blocks
    
    # Calculate how much time has been recorded
    record_time = num_amplifier_samples / header$sample_rate
    
    # Output a summary of contents of header file
    if (data_present > 0) {
        cat(paste("File contains ", record_time, " seconds of data. Amplifiers were sampled at ", header$sample_rate / 1000, " kS/s.\n", sep = ""))
    } else {
        cat(paste("Header file contains no data. Amplifiers were sampled at ", header$sample_rate / 1000, " kS/s.\n", sep = ""))
    }
    
    if (data_present > 0) {
        # Pre-allocate memory for data
        cat("Allocating memory for data...\n")
        
        data = list()
        if ((header$version$major == 1 && header$version$minor >= 2) || (header$version$major > 1)) {
            data$t_amplifier = matrix(0, 1, num_amplifier_samples) # Int32
        } else {
            data$t_amplifier = matrix(0, 1, num_amplifier_samples) # UInt32
        }
        
        data$amplifier_data = allocate_zeros(header$num_amplifier_channels, num_amplifier_samples)
        data$aux_input_data = allocate_zeros(header$num_aux_input_channels, num_aux_input_samples)        
        data$supply_voltage_data = allocate_zeros(header$num_supply_voltage_channels, num_supply_voltage_samples)
        data$temp_sensor_data = allocate_zeros(header$num_temp_sensor_channels, num_supply_voltage_samples)
        data$board_adc_data = allocate_zeros(header$num_board_adc_channels, num_board_adc_samples)
        data$board_dig_in_data = allocate_zeros(header$num_board_dig_in_channels, num_board_dig_in_samples)
        data$board_dig_in_raw = allocate_zeros(1, num_board_dig_in_samples)
        data$board_dig_out_data = allocate_zeros(header$num_board_dig_out_channels, num_board_dig_out_samples)
        data$board_dig_out_raw = allocate_zeros(1, num_board_dig_out_samples)
        
        # Read sampled data from file
        cat("Reading data from file...\n")
        
        # Initialize indices used in looping
        indices = list("amplifier" = 1)
        indices$aux_input = 1
        indices$supply_voltage = 1
        indices$board_adc = 1
        indices$board_dig_in = 1
        indices$board_dig_out = 1
        
        print_increment = 10
        percent_done = print_increment
        for (i in 1:num_data_blocks) {
            data = read_one_data_block(data, header, indices, fid)
            
            # Increment indices
            indices$amplifier = header$num_samples_per_data_block + indices$amplifier
            indices$aux_input = (header$num_samples_per_data_block / 4) + indices$aux_input
            indices$supply_voltage = 1 + indices$supply_voltage
            indices$board_adc = header$num_samples_per_data_block + indices$board_adc
            indices$board_dig_in = header$num_samples_per_data_block + indices$board_dig_in
            indices$board_dig_out = header$num_samples_per_data_block + indices$board_dig_out
            
            fraction_done = 100 * (1.0 * i / num_data_blocks)
            if (fraction_done >= percent_done) {
                cat(paste(percent_done, "% done...\n", sep = ""))
                percent_done = percent_done + print_increment
            }
        }
        
        # Make sure we have read exactly the right amount of data
        bytes_remaining = filesize - seek(fid)
        if (bytes_remaining != 0) {
            stop("Error: End of file not reached.")
        }
    } else {
        data = NA
    }
    
    if (data_present > 0) {
        cat("Parsing data...\n")
        
        # Extract digital input channels to separate variables
        if (header$num_board_dig_in_channels > 0) {
            for (i in 1:header$num_board_dig_in_channels) {
                mask = 2^(header$board_dig_in_channels[[i]]$native_order)
                data$board_dig_in_data[i, 1:ncol(data$board_dig_in_data)] = extract_digital(data$board_dig_in_raw[1, 1:ncol(data$board_dig_in_data)], mask)
            }
        }
        
        # Extract digital output channels to separate variables
        if (header$num_board_dig_out_channels > 0) {
            for (i in 1:header$num_board_dig_out_channels) {
                mask = 2^(header$board_dig_out_channels[[i]]$native_order)
                data$board_dig_out_data[i, 1:ncol(data$board_dig_out_data)] = extract_digital(data$board_dig_out_raw[1, 1:ncol(data$board_dig_out_data)], mask)
            }
        }
        
        # Scale voltage levels appropriately
        data$amplifier_data = 0.195 * (data$amplifier_data - 32768) # units = microvolts
        data$aux_input_data = 37.4e-6 * data$aux_input_data # units = volts
        data$supply_voltage_data = 74.8e-6 * data$supply_voltage_data # units = volts
        
        if (header$eval_board_mode == 1) {
            data$board_adc_data = 152.59e-6 * (data$board_adc_data - 32768) # units = volts
        } else if (header$eval_board_mode == 13) {
            data$board_adc_data = 312.5e-6 * (data$board_adc_data - 32768) # units = volts
        } else {
            data$board_adc_data = 50.354e-6 * data$board_adc_data # units = volts
        }
        
        data$temp_sensor_data = 0.01 * data$temp_sensor_data # units = deg C
        
        # Check for gaps in timestamps
        num_gaps = 0
        
        diff_vector = diff(c(data$t_amplifier))
        for (i in 1:length(diff_vector)) {
            if (diff_vector[i] != 1) {
                num_gaps = num_gaps + 1
            }
        }
        
        if (num_gaps == 0) {
            cat("No missing timestamps in data.\n")
        } else {
            cat(paste("Warning: ", num_gaps, " gaps in timestamp data found. Time scale will not be uniform!\n", sep = ""))
        }
                  
        # Scale time steps (units = seconds)
        data$t_amplifier = data$t_amplifier / header$sample_rate
        max_num_samples = length(data$t_amplifier)
        data$t_aux_input = data$t_amplifier[seq(1, length(data$t_amplifier), 4)]
        data$t_supply_voltage = data$t_amplifier[seq(1, length(data$t_amplifier), header$num_samples_per_data_block)]
        data$t_board_adc = data$t_amplifier
        data$t_dig = data$t_amplifier
        data$t_temp_sensor = data$t_supply_voltage
        
        # If the software notch filter was selected during the recording, apply the same notch filter to amplifier data here
        if (header$frequency_parameters$notch_filter_frequency > 0 && header$version$major < 3) {
            cat("Applying notch filter...\n")
            
            print_increment = 10
            percent_done = print_increment
            for (i in 1:header$num_amplifier_channels) {
                data$amplifier_data[i, 1:ncol(data$amplifier_data)] = notch_filter(data$amplifier_data[i, 1:ncol(data$amplifier_data)], header$sample_rate, header$frequency_parameters$notch_filter_frequency, 10)
                fraction_done = 100 * (i / header$num_amplifier_channels)
                if (fraction_done >= percent_done) {
                    cat(paste(percent_done, "% done...\n", sep=""))
                }
            }
        }
    }
    
    # Move variables to result variable
    result = data_to_result(header, data, data_present)
    
    # End timing
    elapsed = Sys.time() - start
    cat(paste("Done! Elapsed time: ", elapsed, " seconds\n", sep = ""))

    return(list("result" = result, "data_present" = data_present))
}

# Define print_all_channel_names function
print_all_channel_names = function(result) {
    # Print all amplifier_channels
    print_names_in_group(result$amplifier_channels)
    
    # Print all aux_input_channels
    print_names_in_group(result$aux_input_channels)
    
    # Print all supply_voltage_channels
    print_names_in_group(result$supply_voltage_channels)
    
    # Print all board_adc_channels
    print_names_in_group(result$board_adc_channels)
    
    # Print all board_dig_in_channels
    print_names_in_group(result$board_dig_in_channels)
    
    # Print all board_dig_out_channels
    print_names_in_group(result$board_dig_out_channels)
}

# Define function print_names_in_group function
print_names_in_group = function(signal_group) {
    for (this_channel in signal_group) {
        cat(paste(this_channel$custom_channel_name, "\n", sep=""))
    }
}

extract_digital = function(raw_data, mask) {
    mask_vector = rep(mask, length(raw_data))
    intermediate = bitwAnd(raw_data, mask_vector)
    result = c()
    for (i in 1:length(raw_data)) {
        if (intermediate[i]) result[i] = 1 else result[i] = 0
    }
    
    return(result)
}

# Define allocate_zeros
allocate_zeros = function(num_channels, num_samples) {
    if (num_channels > 0) {
        return(matrix(0, num_channels, num_samples))
    } else {
        return(matrix(, nrow=0, ncol=0)) # return empty matrix
    }
}

# Define get_bytes_per_data_block
get_bytes_per_data_block = function(header) {
    # Calculates the number of bytes in each 60 or 128 sample datablock
    
    # Each data block contains 60 or 128 amplifier samples
    bytes_per_block = header$num_samples_per_data_block * 4 # timestamp data
    bytes_per_block = bytes_per_block + header$num_samples_per_data_block * 2 * header$num_amplifier_channels
    
    # Auxiliary inputs are sampled 4x slower than amplifiers
    bytes_per_block = bytes_per_block + (header$num_samples_per_data_block / 4) * 2 * header$num_aux_input_channels
    
    # Supply voltage is sampled 60 or 128x slower than amplifiers
    bytes_per_block = bytes_per_block + 1 * 2 * header$num_supply_voltage_channels
    
    # Board analog inputs are sampled at same rate as amplifers
    bytes_per_block = bytes_per_block + header$num_samples_per_data_block * 2 * header$num_board_adc_channels
    
    # Board digital inputs are sampled at same rate as amplifiers
    if (header$num_board_dig_in_channels > 0) {
        bytes_per_block = bytes_per_block + header$num_samples_per_data_block * 2
    }
    
    # Board digital outputs are sampled at same rate as amplifiers
    if (header$num_board_dig_out_channels > 0) {
        bytes_per_block = bytes_per_block + header$num_samples_per_data_block * 2
    }
    
    # Temp sensor is sampled 60 or 128x slower than amplifiers
    if (header$num_temp_sensor_channels > 0) {
        bytes_per_block = bytes_per_block + 1 * 2 * header$num_temp_sensor_channels
    }
    
    return(bytes_per_block)
}

# Define read_header function
read_header = function(fid) {
    # Check 'magic number' at beginning of file to make sure this is an Intan Technologies RHD2000 data file.
    magic_number = readUInt32(fid)
    if (magic_number != 0xc6912702) {
        stop("Unrecognized file type.")
    }
    
    # Read version number
    version = list(
        major = readInt16(fid),
        minor = readInt16(fid)
    )
    header = list("version" = version)
    
    cat(paste("Reading Intan Technologies RHD2000 Data File, Version ", version$major, ".", version$minor, "\n", sep = ""))
    
    # Read information of sampling rate and amplifier frequency settings
    header$sample_rate = readFloat32(fid)
    
    frequency_parameters = list("dsp_enabled" = readInt16(fid))
    frequency_parameters$actual_dsp_cutoff_frequency = readFloat32(fid)
    frequency_parameters$actual_lower_bandwidth = readFloat32(fid)
    frequency_parameters$actual_upper_bandwidth = readFloat32(fid)
    frequency_parameters$desired_dsp_cutoff_frequency = readFloat32(fid)
    frequency_parameters$desired_lower_bandwidth = readFloat32(fid)
    frequency_parameters$desired_upper_bandwidth = readFloat32(fid)
    
    # This tells us if a software 50/60 Hz notch filter was enabled during the data acquisition.
    notch_filter_mode = readInt16(fid)
    notch_filter_frequency = 0
    if (notch_filter_mode == 1) {
        notch_filter_frequency = 50
    } else if (notch_filter_mode == 2) {
        notch_filter_frequency = 60
    }
    frequency_parameters$notch_filter_frequency = notch_filter_frequency
    frequency_parameters$desired_impedance_test_frequency = readFloat32(fid)
    frequency_parameters$actual_impedance_test_frequency = readFloat32(fid)
    
    # Place notes in array of Strings
    header$notes = c(readQString(fid), readQString(fid), readQString(fid))
    
    # If data file is from GUI v1.1 or later, see if temperature sensor data was saved
    header$num_temp_sensor_channels = 0
    if ((version$major == 1 && version$minor >= 1) || (version$major > 1)) {
        header$num_temp_sensor_channels = readInt16(fid)
    }
    
    # If data file is from GUI v1.3 or later, load eval board mode
    header$eval_board_mode = 0
    if ((version$major == 1 && version$minor >= 3) || (version$major > 1)) {
        header$eval_board_mode = readInt16(fid)
    }
    
    # If data file is from v2.0 or later (Intan Recording Controller), load name of digital reference channel
    header$reference_channel = ""
    if (version$major > 1) {
        header$reference_channel = readQString(fid)
    }
    
    # If data file is from v2.0 or later (Intan Recording Controller), 128 samples in each data block. Otherwise, 60
    header$num_samples_per_data_block = 60
    if (version$major > 1) {
        header$num_samples_per_data_block = 128
    }
    
    # Place frequency-related information in data structure
    frequency_parameters$amplifier_sample_rate = header$sample_rate
    frequency_parameters$aux_input_sample_rate = header$sample_rate / 4
    frequency_parameters$supply_voltage_sample_rate = header$sample_rate / header$num_samples_per_data_block
    frequency_parameters$board_adc_sample_rate = header$sample_rate
    frequency_parameters$board_dig_in_sample_rate = header$sample_rate
    
    header$frequency_parameters = frequency_parameters
    
    header$spike_triggers = list()
    
    header$amplifier_channels = list()
    header$aux_input_channels = list()
    header$supply_voltage_channels = list()
    header$board_adc_channels = list()
    header$board_dig_in_channels = list()
    header$board_dig_out_channels = list()
    
    amplifier_index = 1
    aux_input_index = 1
    supply_voltage_index = 1
    board_adc_index = 1
    board_dig_in_index = 1
    board_dig_out_index = 1
    
    # Read signal summary from data file header
    number_of_signal_groups = readInt16(fid)
    
    for (signal_group in 1:number_of_signal_groups) {
                
        signal_group_name = readQString(fid)
        signal_group_prefix = readQString(fid)
        signal_group_enabled = readInt16(fid)
        signal_group_num_channels = readInt16(fid)
        signal_group_num_amp_channels = readInt16(fid)
        
        if ((signal_group_num_channels > 0) && (signal_group_enabled > 0)) {
            for (signal_channel in 1:signal_group_num_channels) {
                                
                new_channel = list("port_name" = signal_group_name)
                new_channel$port_prefix = signal_group_prefix
                new_channel$port_number = signal_group
                
                new_channel$native_channel_name = readQString(fid)
                new_channel$custom_channel_name = readQString(fid)
                new_channel$native_order = readInt16(fid)
                new_channel$custom_order = readInt16(fid)
                signal_type = readInt16(fid)
                channel_enabled = readInt16(fid)
                new_channel$chip_channel = readInt16(fid)
                new_channel$board_stream = readInt16(fid)
                new_trigger_channel = list("voltage_trigger_mode" = readInt16(fid))
                new_trigger_channel$voltage_threshold = readInt16(fid)
                new_trigger_channel$digital_trigger_channel = readInt16(fid)
                new_trigger_channel$digital_edge_polarity = readInt16(fid)
                new_channel$electrode_impedance_magnitude = readFloat32(fid)
                new_channel$electrode_impedance_phase = readFloat32(fid)
                
                if (channel_enabled > 0) {
                    if (signal_type == 0) {
                        header$amplifier_channels = append(header$amplifier_channels, list(new_channel))
                        header$spike_triggers = append(header$spike_triggers, list(new_trigger_channel))
                        amplifier_index = amplifier_index + 1
                    }
                    else if (signal_type == 1) {
                        header$aux_input_channels = append(header$aux_input_channels, list(new_channel))
                        aux_input_index = aux_input_index + 1
                    }
                    else if (signal_type == 2) {
                        header$supply_voltage_channels = append(header$supply_voltage_channels, list(new_channel))
                        supply_voltage_index = supply_voltage_index + 1
                    }
                    else if (signal_type == 3) {
                        header$board_adc_channels = append(header$board_adc_channels, list(new_channel))
                        board_adc_index = board_adc_index + 1
                    }
                    else if (signal_type == 4) {
                        header$board_dig_in_channels = append(header$board_dig_in_channels, list(new_channel))
                        board_dig_in_index = board_dig_in_index + 1
                    }
                    else if (signal_type == 5) {
                        header$board_dig_out_channels = append(header$board_dig_out_channels, list(new_channel))
                        board_dig_out_index = board_dig_out_index + 1
                    }
                    else {
                        stop("Unknown channel type")
                    }
                }
            }
        }
    }
    
    # Summarize contents of data file
    header$num_amplifier_channels = amplifier_index - 1
    header$num_aux_input_channels = aux_input_index - 1
    header$num_supply_voltage_channels = supply_voltage_index - 1
    header$num_board_adc_channels = board_adc_index - 1
    header$num_board_dig_in_channels = board_dig_in_index - 1
    header$num_board_dig_out_channels = board_dig_out_index - 1
    
    return(header)
}

# Define data_to_result function
data_to_result = function(header, data, data_present) {
    # Moves the header and data (if present) into a common object
    result = list()
    result$t_amplifier = data$t_amplifier
    result$t_aux_input = data$t_aux_input
    result$t_supply_voltage = data$t_supply_voltage
    result$t_board_adc = data$t_board_adc
    result$t_dig = data$t_dig
    result$t_temp_sensor = data$t_temp_sensor
    result$spike_triggers = header$spike_triggers
    
    result$notes = header$notes
    result$frequency_parameters = header$frequency_parameters
    
    result$reference_channel = header$reference_channel
    
    result$amplifier_channels = header$amplifier_channels
    result$amplifier_data = data$amplifier_data
    
    result$aux_input_channels = header$aux_input_channels
    result$aux_input_data = data$aux_input_data
    
    result$supply_voltage_channels = header$supply_voltage_channels
    result$supply_voltage_data = data$supply_voltage_data
    
    result$board_adc_channels = header$board_adc_channels
    result$board_adc_data = data$board_adc_data
    
    result$board_dig_in_channels = header$board_dig_in_channels
    result$board_dig_in_data = data$board_dig_in_data
    
    result$board_dig_out_channels = header$board_dig_out_channels
    result$board_dig_out_data = data$board_dig_out_data
    
    return(result)
}

# Define read_one_data_block function
read_one_data_block = function(data, header, indices, fid) {
    # Reads one 60 or 128 sample data block from fid into data, at the location indicated by indices
    
    # TODO - spoof older version to check both signed and unsigned reading
    # In version 1.2, we moved from saving timestamps as unsigned integers to signed integers to
    # accommodate negative (adjusted) timestamps for pretrigger data
    if ((header$version$major == 1 && header$version$minor >= 2) || (header$version$major > 1)) {
        data$t_amplifier[indices$amplifier:(indices$amplifier + header$num_samples_per_data_block - 1)] = readNInt32(fid, header$num_samples_per_data_block)
    } else {
        data$t_amplifier[indices$amplifier:(indices$amplifier + header$num_samples_per_data_block - 1)] = readNUInt32(fid, header$num_samples_per_data_block)
    }

    if (header$num_amplifier_channels > 0) {
        data$amplifier_data[1:nrow(data$amplifier_data), indices$amplifier:(indices$amplifier + header$num_samples_per_data_block - 1)] = matrix(readNUInt16(fid, nrow(data$amplifier_data) * header$num_samples_per_data_block), nrow=nrow(data$amplifier_data), byrow=TRUE)
    }
    
    if (header$num_aux_input_channels > 0) {
        data$aux_input_data[1:nrow(data$aux_input_data), indices$aux_input:(indices$aux_input + header$num_samples_per_data_block/4 - 1)] = matrix(readNUInt16(fid, nrow(data$aux_input_data) * header$num_samples_per_data_block/4), nrow=nrow(data$aux_input_data), byrow=TRUE)
    }
    
    if (header$num_supply_voltage_channels > 0) {
        data$supply_voltage_data[1:nrow(data$supply_voltage_data), indices$supply_voltage] = readNUInt16(fid, nrow(data$supply_voltage_data))
    }
    
    if (header$num_temp_sensor_channels > 0) {
        data$temp_sensor_data[1:nrow(data$temp_sensor_data), indices$supply_voltage] = readNUInt16(fid, nrow(data$temp_sensor_data))
    }
    
    if (header$num_board_adc_channels > 0) {
        data$board_adc_data[1:nrow(data$board_adc_data), indices$board_adc:(indices$board_adc + header$num_samples_per_data_block - 1)] = matrix(readNUInt16(fid, nrow(data$board_adc_data) * header$num_samples_per_data_block), nrow=nrow(data$board_adc_data), byrow=TRUE)
    }
    
    if (header$num_board_dig_in_channels > 0) {
        data$board_dig_in_raw[1, indices$board_dig_in:(indices$board_dig_in + header$num_samples_per_data_block - 1)] = matrix(readNUInt16(fid, header$num_samples_per_data_block), nrow=nrow(data$board_dig_in_raw), byrow=TRUE)
    }
    
    if (header$num_board_dig_out_channels > 0) {
        data$board_dig_out_raw[1, indices$board_dig_out:(indices$board_dig_out + header$num_samples_per_data_block - 1)] = matrix(readNUInt16(fid, header$num_samples_per_data_block), nrow=nrow(data$board_dig_out_raw), byrow=TRUE)
    }
    
    return(data)
}

# Define notch_filter function
notch_filter = function(input, f_sample, f_notch, bandwidth) {
    t_step = 1 / f_sample
    f_c = f_notch * t_step
    
    l = length(input)
    
    # Calculate IIR filter parameters
    d = exp(-2 * pi * (bandwidth / 2) * t_step)
    b = (1 + d * d) * cos(2 * pi * f_c)
    a0 = 1
    a1 = -b
    a2 = d * d
    a = (1 + d * d) / 2
    b0 = 1
    b1 = -2 * cos(2 * pi * f_c)
    b2 = 1
    
    output = c(input[1], input[2])
    
    # (If filtering a continuous data stream, change output[1] and output[2] to the previous final two values of out.)
    # Run filter
    for (k in 3:l) {
        output[k] = (a*b2*input[k-2] + a*b1*input[k-1] + a*b0*input[k] - a2*output[k-2] - a1*output[k-1])/a0
    }
    return(output)
}

# Define find_channel_in_group function
find_channel_in_group = function(channel_name, signal_group) {
    if (length(signal_group) > 0) {
        for (i in 1:length(signal_group)) {
            if (signal_group[[i]]$custom_channel_name == channel_name) {
                return(list("channel_found" = TRUE, "channel_index" = i))
            }
        }
    }
    return(list("channel_found" = FALSE, "channel_index" = 0))
}

# Define find_channel_in_header function
find_channel_in_header = function(channel_name, header) {
    # Look through all present signal groups
    
    # 1. Look through amplifier_channels
    returnList = find_channel_in_group(channel_name, header$amplifier_channels)
    channel_found = returnList$channel_found
    channel_index = returnList$channel_index
    if (channel_found) {
        return(list("channel_found" = TRUE, "signal_type" = "amplifier_channels", "signal_index" = channel_index))
    }
    
    # 2. Look through aux_input_channels
    returnList = find_channel_in_group(channel_name, header$aux_input_channels)
    channel_found = returnList$channel_found
    channel_index = returnList$channel_index
    if (channel_found) {
        return(list("channel_found" = TRUE, "signal_type" = "aux_input_channels", "signal_index" = channel_index))
    }
    
    # 3. Look through supply_voltage_channels
    returnList = find_channel_in_group(channel_name, header$supply_voltage_channels)
    channel_found = returnList$channel_found
    channel_index = returnList$channel_index
    if (channel_found) {
        return(list("channel_found" = TRUE, "signal_type" = "supply_voltage_channels", "signal_index" = channel_index))
    }
    
    # 4. Look through board_adc_channels
    returnList = find_channel_in_group(channel_name, header$board_adc_channels)
    channel_found = returnList$channel_found
    channel_index = returnList$channel_index
    if (channel_found) {
        return(list("channel_found" = TRUE, "signal_type" = "board_adc_channels", "signal_index" = channel_index))
    }
    
    # 5. Look through board_dig_in_channels
    returnList = find_channel_in_group(channel_name, header$board_dig_in_channels)
    channel_found = returnList$channel_found
    channel_index = returnList$channel_index
    if (channel_found) {
        return(list("channel_found" = TRUE, "signal_type" = "board_dig_in_channels", "signal_index" = channel_index))
    }
    
    # 6. Look through board_dig_out_channels
    returnList = find_channel_in_group(channel_name, header$board_dig_out_channels)
    channel_found = returnList$channel_found
    channel_index = returnList$channel_index
    if (channel_found) {
        return(list("channel_found" = TRUE, "signal_type" = "board_dig_out_channels", "signal_index" = channel_index))
    }
    
    return(list("channel_found" = FALSE, "signal_type" = "", "signal_index" = 0))
}

# Define plot_channel function
plot_channel = function(channel_name, result) {
    # Find channel that corresponds to this name
    returnList = find_channel_in_header(channel_name, result)
    channel_found = returnList$channel_found
    signal_type = returnList$signal_type
    signal_index = returnList$signal_index
    
    # Plot this channel
    if (channel_found) {
        
        if (signal_type == "amplifier_channels") {
            y_label = "Voltage (microVolts)"
            t_vector = result$t_amplifier
            data_vector = result$amplifier_data[signal_index, 1:ncol(result$amplifier_data)]
        } else if (signal_type == "aux_input_channels") {
            y_label = "Voltage (Volts)"
            t_vector = result$t_aux_input
            data_vector = result$aux_input_data[signal_index, 1:ncol(result$aux_input_data)]
        } else if (signal_type == "supply_voltage_channels") {
            y_label = "Voltage (Volts)"
            t_vector = result$t_supply_voltage
            data_vector = result$supply_voltage_data[signal_index, 1:ncol(result$supply_voltage_data)]
        } else if (signal_type == "board_adc_channels") {
            y_label = "Voltage (Volts)"
            t_vector = result$t_board_adc
            data_vector = result$board_adc_data[signal_index, 1:ncol(result$board_adc_data)]
        } else if (signal_type == "board_dig_in_channels") {
            y_label = "Digital In Events (High or Low)"
            t_vector = result$t_dig
            data_vector = result$board_dig_in_data[signal_index, 1:ncol(result$board_dig_in_data)]
        } else if (signal_type == "board_dig_out_channels") {
            y_label = "Digital Out Events (High or Low)"
            t_vector = result$t_dig
            data_vector = result$board_dig_out_data[signal_index, 1:ncol(result$board_dig_out_data)]
        } else {
            stop("Plotting not possible; signal type ", signal_type, " not found")
        }
        
        plot(t_vector, data_vector, type="l", main=channel_name, xlab="Time (s)", ylab=y_label)
    }
    
    else {
        stop(paste("Plotting not possible; channel ", channel_name, " not found", sep = ""))
    }
}

# Define readNInt32 function
readNInt32 = function(fid, nElements) {
    return(replicate(nElements, readInt32(fid)))
}

# Define readNUInt32 function
readNUInt32 = function(fid, nElements) {
    return(replicate(nElements, readUInt32(fid)))
}
                                    
# Define readNUInt16 function
readNUInt16 = function(fid, nElements) {
    return(replicate(nElements, readUInt16(fid)))
}

# Define readFloat32 function
readFloat32 = function(fid) {
    return(readBin(fid, numeric(), n=1, size="4", endian="little"))
}

# Define readUInt16 function
readUInt16 = function(fid) {
    return(sum(2^.subset(0:15, as.logical(rawToBits(readBin(fid, raw(), n=2, endian="little"))))))
}

# Define readUInt32 function
readUInt32 = function(fid) {
    return(sum(2^.subset(0:31, as.logical(rawToBits(readBin(fid, raw(), n=4, endian="little"))))))
}


# Define readInt16 function
readInt16 = function(fid) {
    return(readBin(fid, integer(), n=1, size="2", endian="little"))
}
            
# Define readInt32 function
readInt32 = function(fid) {
    return(readBin(fid, integer(), n=1, size="4", endian="little"))
}

# Define readQString function
readQString = function(fid) {
    # Read Qt style String. The first 32-bit unsigned number indicates the length of the string (in bytes).
    # If this number equals 0xffffffff, the string is null
    a = ""
    length = readUInt32(fid)
    if (length == 0xffffffff) {
        return("")
    }
    
    # Convert length from bytes to 16-bit Unicode words
    length = length / 2
    if (length > 0) {
        for (i in 1:length) {
            thisInt = readUInt16(fid)
            mode(thisInt) = "raw"
            thisChar = sapply(thisInt, rawToChar)
            a = paste(a, thisChar, sep = "")
        }
    }
    return(a)
}

# Define plural function
plural = function(n) {
    # Utility function to optionally pluralize words based on the value of n
    return(if (n == 1) "" else "s")
}