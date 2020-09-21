package com.g2forge.reassert.contract.model.findings.rule;

import com.g2forge.reassert.core.model.contract.terms.TermRelation;
import com.g2forge.reassert.express.explain.model.IExplained;

import lombok.Builder;
import lombok.Data;
import lombok.RequiredArgsConstructor;

@Data
@Builder(toBuilder = true)
@RequiredArgsConstructor
public class SuspiciousUsageFinding implements INoticeFinding {
	protected final IExplained<TermRelation> result;

	protected final String attribute;
}
