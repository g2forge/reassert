package com.g2forge.reassert.git;

import java.time.temporal.ChronoUnit;

import lombok.Builder;
import lombok.Data;
import lombok.RequiredArgsConstructor;

@Data
@Builder(toBuilder = true)
@RequiredArgsConstructor
public class GitConfig {
	protected final int amount;
	
	protected final ChronoUnit unit;
}
