package com.g2forge.reassert.express.v2.eval.integer;

import java.util.function.BinaryOperator;
import java.util.stream.Collectors;

import org.junit.Test;

import com.g2forge.alexandria.test.HAssert;
import com.g2forge.reassert.express.v2.eval.IEvaluator;
import com.g2forge.reassert.express.v2.eval.ValueEvaluator;
import com.g2forge.reassert.express.v2.model.constant.ILiteral;
import com.g2forge.reassert.express.v2.model.constant.Literal;
import com.g2forge.reassert.express.v2.model.environment.Environment;
import com.g2forge.reassert.express.v2.model.operation.ArithmeticOperation;
import com.g2forge.reassert.express.v2.model.operation.IOperation;
import com.g2forge.reassert.express.v2.model.operation.IOperation.IOperationBuilder;
import com.g2forge.reassert.express.v2.model.variable.Closure;
import com.g2forge.reassert.express.v2.model.variable.Variable;

import lombok.Getter;

public class TestIntegerOperationSystem {
	protected static final int[] numbers = new int[] { 10, 0, -3, 1, 5, -17 };

	@Getter(lazy = true)
	private static final IEvaluator<String, Integer, Integer> evaluator = new ValueEvaluator<>(IntegerValueSystem.create(), IntegerOperationSystem.create());

	protected static void reduction(ArithmeticOperation.Operator operator, BinaryOperator<Integer> accumulator) {
		for (int i = 0; i < (1 << numbers.length); i++) {
			final IOperationBuilder<String, Integer, ?, ?> builder = operator.<String, Integer>builder();
			Integer expected = IntegerOperationSystem.create().getDescriptor(operator).getIdentity().get();
			boolean first = true;
			for (int j = 0; j < numbers.length; j++) {
				if (((i >>> j) & 0x1) == 0) continue;

				final int value = numbers[j];
				builder.argument$(value);

				if (first) {
					expected = value;
					first = false;
				} else expected = accumulator.apply(expected, value);
			}
			final IOperation<String, Integer> actual = builder.valid();

			@SuppressWarnings("unchecked")
			final String argumentsString = actual.getArguments().stream().map(e -> (ILiteral<?, Integer>) e).map(ILiteral::get).map(Object::toString).collect(Collectors.joining(", "));
			HAssert.assertEquals(String.format("%1$s failed with arguments: %2$s", operator, argumentsString), expected, getEvaluator().eval(actual));
		}
	}

	@Test
	public void add() {
		reduction(ArithmeticOperation.Operator.ADD, (a, b) -> a + b);
	}

	@Test
	public void apply() {
		final Variable<String, Integer> x = new Variable<>("x");
		HAssert.assertEquals(Integer.valueOf(0), getEvaluator().eval(new Closure<>(Environment.<String, Integer>builder().bind(x, new Literal<>(0)).build(), x)));
	}

	@Test
	public void divide() {
		HAssert.assertEquals(Integer.valueOf(2), getEvaluator().eval(ArithmeticOperation.Operator.DIVIDE.<String, Integer>builder().argument$(10).argument$(5).valid()));
	}

	@Test
	public void literal() {
		HAssert.assertEquals(Integer.valueOf(0), getEvaluator().eval(new Literal<>("", 0)));
		HAssert.assertEquals(Integer.valueOf(1), getEvaluator().eval(new Literal<>("", 1)));
	}

	@Test
	public void multiply() {
		reduction(ArithmeticOperation.Operator.MULTIPLY, (a, b) -> a * b);
	}

	@Test
	public void subtract() {
		HAssert.assertEquals(Integer.valueOf(-1), getEvaluator().eval(ArithmeticOperation.Operator.SUBTRACT.<String, Integer>builder().argument$(0).argument$(1).valid()));
	}
}
