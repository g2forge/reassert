package com.g2forge.reassert.reassert.summary.model;

import org.slf4j.event.Level;

import com.g2forge.reassert.core.model.contract.license.ILicense;
import com.g2forge.reassert.core.model.contract.usage.IUsage;
import com.g2forge.reassert.core.model.coordinates.ICoordinates;
import com.g2forge.reassert.term.analyze.model.findings.IRiskFinding;

import lombok.Builder;
import lombok.Data;
import lombok.RequiredArgsConstructor;

@Data
@Builder(toBuilder = true)
@RequiredArgsConstructor
public class RiskSummary {
	protected final Level level;
	
	protected final ICoordinates artifact;

	protected final IRiskFinding risk;
	
	protected final IUsage usage;

	protected final ILicense license;
}
