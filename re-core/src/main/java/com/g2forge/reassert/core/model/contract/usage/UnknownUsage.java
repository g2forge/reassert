package com.g2forge.reassert.core.model.contract.usage;

import lombok.Builder;
import lombok.Data;
import lombok.RequiredArgsConstructor;

/**
 * Represent a usage we do not yet recognize, but have some kind of text for.
 */
@Data
@Builder(toBuilder = true)
@RequiredArgsConstructor
public class UnknownUsage implements IUsageApplied {
	protected final String text;
}
