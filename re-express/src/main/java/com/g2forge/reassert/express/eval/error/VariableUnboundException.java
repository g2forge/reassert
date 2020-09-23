package com.g2forge.reassert.express.eval.error;

import java.util.NoSuchElementException;

import com.g2forge.reassert.express.model.variable.IVariable;

import lombok.Getter;

@Getter
public class VariableUnboundException extends NoSuchElementException implements IExpressionException {
	private static final long serialVersionUID = -7062004322384292955L;

	protected final IVariable<?, ?> expression;

	public VariableUnboundException(IVariable<?, ?> variable) {
		super(String.format("%1$s is not bound", variable.getName()));
		this.expression = variable;
	}
}
