package com.g2forge.reassert.git;

import java.time.temporal.ChronoUnit;

import lombok.Builder;
import lombok.Data;
import lombok.RequiredArgsConstructor;

@Data
@Builder(toBuilder = true)
@RequiredArgsConstructor
public class GitConfig {
	@Builder.Default
	protected final int amount = 1;

	@Builder.Default
	protected final ChronoUnit unit = ChronoUnit.DAYS;
}
