package com.g2forge.reassert.term.analyze.model.findings;

import com.g2forge.reassert.core.model.contract.TermRelation;
import com.g2forge.reassert.term.eee.explain.model.IExplained;

import lombok.Builder;
import lombok.Data;
import lombok.RequiredArgsConstructor;

@Data
@Builder(toBuilder = true)
@RequiredArgsConstructor
public class DiscloseSourceFinding implements IRiskFinding {
	protected final IExplained<TermRelation> result;

	@Override
	public String getDescription() {
		return "You must disclose the source for this artifact";
	}
}
