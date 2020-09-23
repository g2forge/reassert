package com.g2forge.reassert.standard.model.contract.usage;

import com.g2forge.reassert.contract.model.finding.rule.SuspiciousUsageFinding;
import com.g2forge.reassert.core.api.ReassertLegalOpinion;

import lombok.Getter;
import lombok.RequiredArgsConstructor;

@ReassertLegalOpinion
@Getter
@RequiredArgsConstructor
public enum StandardUsageTermAttribute implements SuspiciousUsageFinding.ITermsAttribute {
	Distribution("method of distribution"),
	Consumption("method of consumption"),
	Format("format");

	protected final String description;
}
