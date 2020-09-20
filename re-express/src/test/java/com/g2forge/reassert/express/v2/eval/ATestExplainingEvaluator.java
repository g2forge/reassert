package com.g2forge.reassert.express.v2.eval;

import org.junit.Test;

import com.g2forge.alexandria.java.fluent.optional.NullableOptional;
import com.g2forge.alexandria.test.HAssert;
import com.g2forge.reassert.express.v2.model.IExplained;
import com.g2forge.reassert.express.v2.model.IExplained.Relevance;
import com.g2forge.reassert.express.v2.model.constant.Literal;
import com.g2forge.reassert.express.v2.model.constant.NoValueConstant;
import com.g2forge.reassert.express.v2.model.environment.Environment;
import com.g2forge.reassert.express.v2.model.operation.ExplainedOperation;
import com.g2forge.reassert.express.v2.model.operation.IOperation;
import com.g2forge.reassert.express.v2.model.operation.ZeroExplainedOperation;
import com.g2forge.reassert.express.v2.model.variable.Closure;
import com.g2forge.reassert.express.v2.model.variable.ExplainedClosure;
import com.g2forge.reassert.express.v2.model.variable.ExplainedVariable;
import com.g2forge.reassert.express.v2.model.variable.Variable;

public abstract class ATestExplainingEvaluator<Value> {
	@Test
	public void closure() {
		final Variable<String, Value> x = new Variable<>("x");

		final NullableOptional<IExplained<Value>> value = NullableOptional.of(new Literal<>(getZero()));
		final IExplained<Value> expected = ExplainedClosure.<String, Value>builder().expression(new ExplainedVariable<>(x, value)).binding$(IExplained.Relevance.Dominant, x, value).build();

		final IExplained<Value> actual = getEvaluator().eval(new Closure<>(Environment.<String, Value>builder().bind$(x, getZero()).build(), x));

		HAssert.assertEquals(expected, actual);
	}

	public abstract IEvaluator<String, Value, IExplained<Value>> getEvaluator();

	public abstract IOperation.IOperator getMultiply();

	public abstract Value getOne();

	public abstract Value getZero();

	@Test
	public void literal() {
		final Literal<String, Value> x = new Literal<>("x", getZero());
		HAssert.assertEquals(x, getEvaluator().eval(x));
	}

	@Test(expected = RuntimeException.class)
	public void operationException() {
		getEvaluator().eval(getMultiply().<String, Value>builder().argument(new NoValueConstant<>()).build());
	}

	@Test
	public void operationIdentity() {
		final IExplained<Value> expected = ExplainedOperation.<Value>builder().operator(getMultiply()).value(getOne()).identity$(getOne()).argument$(Relevance.Identity, getOne()).argument$(Relevance.Identity, getOne()).build();
		final IExplained<Value> actual = getEvaluator().eval(getMultiply().<String, Value>builder().argument$(getOne()).argument$(getOne()).build());
		HAssert.assertEquals(expected, actual);
	}

	@Test
	public void operationZero1() {
		final IExplained<Value> expected = ZeroExplainedOperation.<Value>builder().operator(getMultiply()).zero(getZero()).argument$(Relevance.Dominant, getZero()).build();
		final IExplained<Value> actual = getEvaluator().eval(getMultiply().<String, Value>builder().argument$(getZero()).build());
		HAssert.assertEquals(expected, actual);
	}

	@Test
	public void operationZero2() {
		final IExplained<Value> expected = ZeroExplainedOperation.<Value>builder().operator(getMultiply()).zero(getZero()).argument$(Relevance.Dominant, getZero()).argument$(Relevance.Unevaluated, getOne()).build();
		final IExplained<Value> actual = getEvaluator().eval(getMultiply().<String, Value>builder().argument$(getZero()).argument$(getOne()).build());
		HAssert.assertEquals(expected, actual);
	}

	@Test
	public void variable() {
		final Variable<String, Value> x = new Variable<>("x");
		HAssert.assertEquals(new ExplainedVariable<>(x, NullableOptional.empty()), getEvaluator().eval(x));
	}
}
