package com.g2forge.reassert.contract.v2.model.finding;

import java.util.Collection;

import com.g2forge.reassert.contract.model.IExpressionContext;
import com.g2forge.reassert.core.model.contract.terms.ITerm;
import com.g2forge.reassert.core.model.contract.terms.TermRelation;
import com.g2forge.reassert.core.model.report.IContextFinding;
import com.g2forge.reassert.core.model.report.IFinding;
import com.g2forge.reassert.express.express.IExpression;

import lombok.Builder;
import lombok.Data;
import lombok.RequiredArgsConstructor;

@Data
@Builder(toBuilder = true)
@RequiredArgsConstructor
public class ExpressionContextFinding implements IContextFinding, IExpressionContext {
	protected final Collection<ITerm> inputs;

	protected final IExpression<TermRelation> expression;

	protected final Collection<ITerm> outputs;

	protected final IFinding finding;
}
