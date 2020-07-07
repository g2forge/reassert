package com.g2forge.reassert.term.analyze.model.findings;

import com.g2forge.reassert.core.model.contract.TermRelation;
import com.g2forge.reassert.term.eee.explain.model.IExplained;

import lombok.Builder;
import lombok.Data;
import lombok.RequiredArgsConstructor;

@Data
@Builder(toBuilder = true)
@RequiredArgsConstructor
public class SuspiciousUsageFinding implements IRiskFinding {
	protected final IExplained<TermRelation> result;

	@Override
	public String getDescription() {
		return "The usage for this artifact may not be properly specified";
	}
}
