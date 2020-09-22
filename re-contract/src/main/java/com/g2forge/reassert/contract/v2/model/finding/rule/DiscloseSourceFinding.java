package com.g2forge.reassert.contract.v2.model.finding.rule;

import com.g2forge.reassert.core.model.contract.terms.TermRelation;
import com.g2forge.reassert.express.v2.model.IExplained;

import lombok.Builder;
import lombok.Data;
import lombok.RequiredArgsConstructor;

@Data
@Builder(toBuilder = true)
@RequiredArgsConstructor
public class DiscloseSourceFinding implements INoticeFinding {
	protected final IExplained<TermRelation> result;
}
