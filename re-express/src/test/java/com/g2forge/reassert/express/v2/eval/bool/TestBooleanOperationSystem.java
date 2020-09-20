package com.g2forge.reassert.express.v2.eval.bool;

import java.util.function.BinaryOperator;
import java.util.stream.Collectors;

import org.junit.Test;

import com.g2forge.alexandria.test.HAssert;
import com.g2forge.reassert.express.v2.eval.IEvaluator;
import com.g2forge.reassert.express.v2.eval.ValueEvaluator;
import com.g2forge.reassert.express.v2.model.constant.ILiteral;
import com.g2forge.reassert.express.v2.model.constant.Literal;
import com.g2forge.reassert.express.v2.model.constant.NoValueConstant;
import com.g2forge.reassert.express.v2.model.environment.Environment;
import com.g2forge.reassert.express.v2.model.operation.BooleanOperation;
import com.g2forge.reassert.express.v2.model.operation.IOperation;
import com.g2forge.reassert.express.v2.model.operation.IOperation.IOperationBuilder;
import com.g2forge.reassert.express.v2.model.variable.Closure;
import com.g2forge.reassert.express.v2.model.variable.Variable;

import lombok.Getter;

public class TestBooleanOperationSystem {
	@Getter(lazy = true)
	private static final IEvaluator<String, Boolean, Boolean> evaluator = new ValueEvaluator<>(BooleanValueSystem.create(), BooleanOperationSystem.create());

	protected static void reduction(BooleanOperation.Operator operator, BinaryOperator<Boolean> accumulator) {
		for (int n = 1; n < 6; n++) {
			for (int i = 0; i < (1 << n); i++) {
				final IOperationBuilder<String, Boolean, ?, ?> builder = operator.<String, Boolean>builder();
				boolean expected = false;
				for (int j = 0; j < n; j++) {
					final boolean value = ((i >>> j) & 0x1) != 0;
					builder.argument$(value);

					if (j == 0) expected = (i & 0x1) != 0;
					else expected = accumulator.apply(expected, value);
				}
				final IOperation<String, Boolean> actual = builder.valid();

				@SuppressWarnings("unchecked")
				final String argumentsString = actual.getArguments().stream().map(e -> (ILiteral<?, Boolean>) e).map(ILiteral::get).map(Object::toString).collect(Collectors.joining(", "));
				HAssert.assertEquals(String.format("Failed with %1$d arguments to %2$s: %3$s", n, operator, argumentsString), expected, getEvaluator().eval(actual));
			}
		}
	}

	@Test
	public void and() {
		reduction(BooleanOperation.Operator.AND, Boolean::logicalAnd);
	}

	@Test
	public void apply() {
		final Variable<String, Boolean> x = new Variable<>("x");
		HAssert.assertEquals(false, getEvaluator().eval(new Closure<>(Environment.<String, Boolean>builder().bind(x, new Literal<>(false)).build(), x)));
	}

	@Test
	public void literal() {
		HAssert.assertEquals(false, getEvaluator().eval(new Literal<>("", false)));
		HAssert.assertEquals(true, getEvaluator().eval(new Literal<>("", true)));
	}

	@Test
	public void not() {
		HAssert.assertEquals(true, getEvaluator().eval(BooleanOperation.Operator.NOT.<String, Boolean>builder().argument$(false).valid()));
		HAssert.assertEquals(false, getEvaluator().eval(BooleanOperation.Operator.NOT.<String, Boolean>builder().argument$(true).valid()));
	}

	@Test
	public void or() {
		reduction(BooleanOperation.Operator.OR, Boolean::logicalOr);
	}

	@Test
	public void skipAnd() {
		HAssert.assertEquals(false, getEvaluator().eval(BooleanOperation.Operator.AND.<String, Boolean>builder().argument$(false).argument(new NoValueConstant<>()).build()));
	}

	@Test
	public void skipOr() {
		HAssert.assertEquals(true, getEvaluator().eval(BooleanOperation.Operator.OR.<String, Boolean>builder().argument$(true).argument(new NoValueConstant<>()).build()));
	}

	@Test
	public void xor() {
		reduction(BooleanOperation.Operator.XOR, Boolean::logicalXor);
	}
}
