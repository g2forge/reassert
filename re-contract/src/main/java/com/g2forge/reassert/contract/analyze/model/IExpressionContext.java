package com.g2forge.reassert.contract.analyze.model;

import java.util.Collection;

import com.g2forge.reassert.core.model.contract.ITerm;
import com.g2forge.reassert.core.model.contract.TermRelation;
import com.g2forge.reassert.expression.express.IExpression;

public interface IExpressionContext {
	public IExpression<TermRelation> getExpression();
	
	public Collection<ITerm> getInputs();

	public Collection<ITerm> getOutputs();
}
