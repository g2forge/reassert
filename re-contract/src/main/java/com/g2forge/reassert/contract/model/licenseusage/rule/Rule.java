package com.g2forge.reassert.contract.model.licenseusage.rule;

import com.g2forge.reassert.contract.model.finding.IFindingFactory;
import com.g2forge.reassert.contract.model.licenseusage.ICTName;
import com.g2forge.reassert.core.model.contract.terms.TermRelation;
import com.g2forge.reassert.express.model.IExpression;

import lombok.Builder;
import lombok.Data;
import lombok.RequiredArgsConstructor;

@Data
@Builder(toBuilder = true)
@RequiredArgsConstructor
public class Rule implements IRule {
	protected final IExpression<ICTName, TermRelation> expression;

	protected final IFindingFactory<?> finding;
}
