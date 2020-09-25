package com.g2forge.reassert.express.eval.bool;

import java.util.function.BinaryOperator;
import java.util.stream.Collectors;

import org.junit.Test;

import com.g2forge.alexandria.java.validate.ValidationFailureException;
import com.g2forge.alexandria.test.HAssert;
import com.g2forge.reassert.express.eval.IEvaluator;
import com.g2forge.reassert.express.eval.ValueEvaluator;
import com.g2forge.reassert.express.eval.operation.BooleanOperationSystem;
import com.g2forge.reassert.express.eval.value.ObjectValueSystem;
import com.g2forge.reassert.express.model.IExpression;
import com.g2forge.reassert.express.model.constant.ILiteral;
import com.g2forge.reassert.express.model.constant.Literal;
import com.g2forge.reassert.express.model.constant.NoValueConstant;
import com.g2forge.reassert.express.model.environment.Environment;
import com.g2forge.reassert.express.model.operation.BooleanOperation;
import com.g2forge.reassert.express.model.operation.IOperation;
import com.g2forge.reassert.express.model.operation.IOperation.IOperationBuilder;
import com.g2forge.reassert.express.model.variable.Closure;
import com.g2forge.reassert.express.model.variable.Variable;

import lombok.Getter;

public class TestBooleanOperationSystem {
	@Getter(lazy = true)
	private static final IEvaluator<String, Boolean, Boolean> evaluator = new ValueEvaluator<>(ObjectValueSystem.create(), BooleanOperationSystem.create());

	protected static void reduction(BooleanOperation.Operator operator, BinaryOperator<Boolean> accumulator) {
		for (int n = 1; n < 6; n++) {
			for (int i = 0; i < (1 << n); i++) {
				final IOperationBuilder<String, Boolean, ?, ?> builder = operator.<String, Boolean>builder();
				boolean expected = false;
				for (int j = 0; j < n; j++) {
					final boolean value = ((i >>> j) & 0x1) != 0;
					builder.argument$L(value);

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
	public void applySimple() {
		final Variable<String, Boolean> x = new Variable<>("x");
		HAssert.assertEquals(false, getEvaluator().eval(new Closure<>(Environment.<String, Boolean>builder().bind(x, new Literal<>(false)).build(), x)));
	}

	@Test
	public void applyZero() {
		final Variable<String, Boolean> x = new Variable<>("x"), y = new Variable<>("y");
		final IExpression<String, Boolean> expression = BooleanOperation.and(x, y);
		final IExpression<String, Boolean> closure = new Closure<>(Environment.<String, Boolean>builder().bind(x, new Literal<>(false)).build(), expression);
		HAssert.assertEquals(false, TestBooleanOperationSystem.getEvaluator().eval(closure));
	}

	@Test
	public void implies() {
		for (int i = 0; i < 4; i++) {
			final BooleanOperation<String, Boolean> expression = BooleanOperation.implies(new Literal<>((i & 0x02) != 0), new Literal<>((i & 0x01) != 0));
			final Boolean result = getEvaluator().eval(expression);
			HAssert.assertEquals(expression.toString(), i != 2, result);
		}
	}

	@Test(expected = ValidationFailureException.class)
	public void invalid() {
		getEvaluator().eval(new Literal<>(null));
	}

	@Test
	public void literal() {
		HAssert.assertEquals(false, getEvaluator().eval(new Literal<>("", false)));
		HAssert.assertEquals(true, getEvaluator().eval(new Literal<>("", true)));
	}

	@Test
	public void not() {
		HAssert.assertEquals(true, getEvaluator().eval(BooleanOperation.Operator.NOT.<String, Boolean>builder().argument$L(false).valid()));
		HAssert.assertEquals(false, getEvaluator().eval(BooleanOperation.Operator.NOT.<String, Boolean>builder().argument$L(true).valid()));
	}

	@Test
	public void or() {
		reduction(BooleanOperation.Operator.OR, Boolean::logicalOr);
	}

	@Test
	public void skipAnd() {
		HAssert.assertEquals(false, getEvaluator().eval(BooleanOperation.Operator.AND.<String, Boolean>builder().argument$L(false).argument(new NoValueConstant<>()).build()));
	}

	@Test
	public void skipOr() {
		HAssert.assertEquals(true, getEvaluator().eval(BooleanOperation.Operator.OR.<String, Boolean>builder().argument$L(true).argument(new NoValueConstant<>()).build()));
	}

	@Test
	public void xor() {
		reduction(BooleanOperation.Operator.XOR, Boolean::logicalXor);
	}
}
