package com.g2forge.reassert.term.analyze.model.findings;

import java.util.Collection;

import com.g2forge.reassert.core.model.contract.ITerm;
import com.g2forge.reassert.core.model.contract.TermRelation;
import com.g2forge.reassert.core.model.report.IContextualFinding;
import com.g2forge.reassert.core.model.report.IFinding;
import com.g2forge.reassert.term.analyze.model.IExpressionContext;
import com.g2forge.reassert.term.eee.express.IExpression;

import lombok.Builder;
import lombok.Data;
import lombok.RequiredArgsConstructor;

@Data
@Builder(toBuilder = true)
@RequiredArgsConstructor
public class ExpressionContextualFinding implements IContextualFinding, IExpressionContext {
	protected final Collection<ITerm> inputs;

	protected final IExpression<TermRelation> expression;

	protected final Collection<ITerm> outputs;

	protected final IFinding finding;
}
