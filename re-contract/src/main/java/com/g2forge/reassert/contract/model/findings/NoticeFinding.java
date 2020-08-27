package com.g2forge.reassert.contract.model.findings;

import com.g2forge.reassert.contract.model.findings.IRiskFinding;
import com.g2forge.reassert.core.model.contract.terms.TermRelation;
import com.g2forge.reassert.expression.explain.model.IExplained;

import lombok.Builder;
import lombok.Data;
import lombok.RequiredArgsConstructor;

@Data
@Builder(toBuilder = true)
@RequiredArgsConstructor
public class NoticeFinding implements IRiskFinding {
	protected final IExplained<TermRelation> result;
}
