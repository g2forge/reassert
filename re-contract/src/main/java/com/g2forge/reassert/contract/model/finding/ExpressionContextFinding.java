package com.g2forge.reassert.contract.model.finding;

import java.util.Set;

import com.g2forge.reassert.contract.model.name.IContractComparisonName;
import com.g2forge.reassert.core.model.contract.terms.ITerm;
import com.g2forge.reassert.core.model.contract.terms.TermRelation;
import com.g2forge.reassert.core.model.report.IContextFinding;
import com.g2forge.reassert.core.model.report.IFinding;
import com.g2forge.reassert.express.model.IExpression;

import lombok.Builder;
import lombok.Data;
import lombok.RequiredArgsConstructor;

@Data
@Builder(toBuilder = true)
@RequiredArgsConstructor
public class ExpressionContextFinding implements IContextFinding {
	protected final IExpression<IContractComparisonName, TermRelation> expression;

	protected final Set<ITerm> inputs;

	protected final Set<ITerm> outputs;
	
	protected final IFinding finding;
}
