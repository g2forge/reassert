package com.g2forge.reassert.contract.algorithm.licenseusage.model.finding;

import com.g2forge.reassert.contract.model.finding.INoticeFinding;
import com.g2forge.reassert.core.model.contract.terms.TermRelation;
import com.g2forge.reassert.express.model.IExplained;

import lombok.Builder;
import lombok.Data;
import lombok.RequiredArgsConstructor;

@Data
@Builder(toBuilder = true)
@RequiredArgsConstructor
public class CopyrightNoticeFinding implements INoticeFinding {
	protected final IExplained<TermRelation> result;
}
