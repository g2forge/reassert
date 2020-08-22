package com.g2forge.reassert.contract.analyze.model.findings;

import com.g2forge.reassert.contract.analyze.model.findings.IRiskFinding;
import com.g2forge.reassert.contract.eee.explain.model.IExplained;
import com.g2forge.reassert.core.model.contract.TermRelation;

import lombok.Builder;
import lombok.Data;
import lombok.RequiredArgsConstructor;

@Data
@Builder(toBuilder = true)
@RequiredArgsConstructor
public class StateChangesFinding implements IRiskFinding {
	protected final IExplained<TermRelation> result;
}
