package com.g2forge.reassert.contract.convert;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import org.slf4j.event.Level;

import com.g2forge.alexandria.java.close.ICloseable;
import com.g2forge.alexandria.java.core.enums.EnumException;
import com.g2forge.alexandria.java.core.error.HError;
import com.g2forge.alexandria.java.core.helpers.HCollection;
import com.g2forge.alexandria.java.function.IConsumer2;
import com.g2forge.alexandria.java.type.function.TypeSwitch1;
import com.g2forge.enigma.backend.convert.ARenderer;
import com.g2forge.enigma.backend.convert.IExplicitRenderable;
import com.g2forge.enigma.backend.convert.IRendering;
import com.g2forge.enigma.backend.convert.textual.ATextualRenderer;
import com.g2forge.enigma.backend.text.model.modifier.TextNestedModified;
import com.g2forge.reassert.contract.model.IExpressionContext;
import com.g2forge.reassert.contract.model.TermConstant;
import com.g2forge.reassert.contract.model.TermType;
import com.g2forge.reassert.contract.model.findings.ConditionFinding;
import com.g2forge.reassert.contract.model.findings.DiscloseSourceFinding;
import com.g2forge.reassert.contract.model.findings.ExpressionContextualFinding;
import com.g2forge.reassert.contract.model.findings.IRiskFinding;
import com.g2forge.reassert.contract.model.findings.NoticeFinding;
import com.g2forge.reassert.contract.model.findings.StateChangesFinding;
import com.g2forge.reassert.contract.model.findings.SuspiciousUsageFinding;
import com.g2forge.reassert.contract.model.findings.UnrecognizedTermFinding;
import com.g2forge.reassert.core.model.contract.ITerm;
import com.g2forge.reassert.core.model.contract.license.ILicenseTerm;
import com.g2forge.reassert.core.model.contract.license.MultiLicenseFinding;
import com.g2forge.reassert.core.model.contract.usage.IUsageTerm;
import com.g2forge.reassert.core.model.report.IContextualFinding;
import com.g2forge.reassert.core.model.report.IFinding;
import com.g2forge.reassert.core.model.report.IReport;
import com.g2forge.reassert.core.model.work.IncompatibleWorkLicenseFinding;
import com.g2forge.reassert.core.model.work.UnknownWorkTypeFinding;
import com.g2forge.reassert.expression.explain.convert.ExplanationMode;
import com.g2forge.reassert.expression.explain.convert.ExplanationRenderer;
import com.g2forge.reassert.expression.explain.model.IExplained;
import com.g2forge.reassert.expression.express.IConstant;
import com.g2forge.reassert.expression.express.IExpression;
import com.g2forge.reassert.expression.express.Operation;

import lombok.AccessLevel;
import lombok.Getter;
import lombok.RequiredArgsConstructor;

@Getter
@RequiredArgsConstructor
public class ReportRenderer extends ATextualRenderer<Object, IReportRenderContext> {
	@Getter(AccessLevel.PROTECTED)
	protected class ReportRenderContext extends ARenderContext implements IReportRenderContext {
		@Getter(AccessLevel.PUBLIC)
		protected final ExplanationMode mode;

		protected final ExplanationRenderer explanationRenderer;

		protected final IConsumer2<TextNestedModified.TextNestedModifiedBuilder, Object> valueContext;

		@Getter(AccessLevel.PUBLIC)
		protected IExpressionContext findingContext;

		public ReportRenderContext(TextNestedModified.TextNestedModifiedBuilder builder, ExplanationMode mode, IConsumer2<TextNestedModified.TextNestedModifiedBuilder, Object> valueContext) {
			super(builder);
			this.mode = mode;
			this.explanationRenderer = new ExplanationRenderer(getMode());
			this.valueContext = valueContext;
		}

		@Override
		public ICloseable findingContext(IExpressionContext findingContext) {
			final IExpressionContext previous = this.findingContext;
			this.findingContext = findingContext;
			return () -> this.findingContext = previous;
		}

		@Override
		protected IReportRenderContext getThis() {
			return this;
		}

		@Override
		public IReportRenderContext render(IExplained<?> explained) {
			explanationRenderer.render(getBuilder(), explained);
			return this;
		}
	}

	protected static class ReportRendering extends ARenderer.ARendering<Object, IReportRenderContext, IExplicitRenderable<? super IReportRenderContext>> {
		protected IReportRenderContext appendLevel(IFinding finding, IReportRenderContext context) {
			if (context.getMode().compareTo(ExplanationMode.Describe) >= 0) context.append(finding.getLevel()).append(": ");
			return context;
		}

		protected IReportRenderContext explain(IRiskFinding finding, IReportRenderContext context) {
			if (context.getMode().compareTo(ExplanationMode.Describe) >= 0) {
				if ((finding.getLevel().compareTo(Level.WARN) <= 0) || (context.getMode().compareTo(ExplanationMode.Explain) >= 0)) try (final ICloseable indent = context.newline().indent()) {
					final IExpressionContext findingContext = context.getFindingContext();
					if (findingContext != null) context.append("Rule: ").render(findingContext.getExpression(), IExpression.class).newline();
					context.append("Explanation: ").render(finding.getResult());
				}
			}
			return context;
		}

		@Override
		protected void extend(TypeSwitch1.FunctionBuilder<Object, IExplicitRenderable<? super IReportRenderContext>> builder) {
			builder.add(IExplicitReportRenderable.class, e -> c -> ((IExplicitReportRenderable<?>) e).render(c));
			builder.add(IExplained.class, e -> c -> c.render(e));

			builder.add(IReport.class, e -> c -> {
				c.append("Minimum finding level: ");
				final Collection<IFinding> findings = e.getFindings();
				if (findings.isEmpty()) c.append("NONE").newline();
				else {
					c.append(e.getMinLevel()).newline();
					if (findings != null) for (IFinding finding : findings) {
						c.render(finding, IFinding.class).newline();
					}
				}
			});

			builder.add(ILicenseTerm.class, e -> c -> c.append('\"').append(e.getDescription()).append('\"'));
			builder.add(IUsageTerm.class, e -> c -> c.append('\"').append(e.getDescription()).append('\"'));

			builder.add(IConstant.class, e -> c -> c.append('(').append(e.getName()).append(')'));
			builder.add(TermConstant.class, e -> c -> c.render(e.getTerm(), ITerm.class));
			builder.add(Operation.class, e -> c -> {
				final Operation<?> castE = (Operation<?>) e;
				c.append('(');
				final String separator;
				switch (castE.getOperator()) {
					case NOT:
						c.append('!').render(HCollection.getOne(castE.getArguments()), IExpression.class).append(')');
						return;
					case AND:
						separator = "&&";
						break;
					case OR:
						separator = "||";
						break;
					default:
						throw new EnumException(Operation.Operator.class, castE.getOperator());
				}

				boolean first = true;
				for (IExpression<?> argument : castE.getArguments()) {
					if (first) first = false;
					else c.append(' ').append(separator).append(' ');
					c.render(argument, IExpression.class);
				}
				c.append(')');
			});

			builder.add(IContextualFinding.class, e -> c -> c.render(e.getFinding(), IFinding.class));
			builder.add(ExpressionContextualFinding.class, e -> c -> {
				try (final ICloseable findingContext = c.findingContext(e)) {
					c.render(e.getFinding(), IFinding.class);
				}
			});

			builder.add(IncompatibleWorkLicenseFinding.class, e -> c -> {
				appendLevel(e, c).append("A license is incompatible with a combined work that includes artifacts under this license");
				if (c.getMode().compareTo(ExplanationMode.Describe) >= 0) try (final ICloseable indent = c.indent()) {
					if ((e.getUnknown() != null) && !e.getUnknown().isEmpty()) {
						c.newline().append("License terms with unknown compatability: ");
						final List<ILicenseTerm> unknown = new ArrayList<>(e.getUnknown());
						final int size = unknown.size();
						for (int i = 0; i < size; i++) {
							if (i > 0) c.append((i == size - 1) ? " & " : ", ");
							c.render(unknown.get(i), ILicenseTerm.class);
						}
					}
					if ((e.getMismatched() != null) && !e.getMismatched().isEmpty()) {
						c.newline().append("Mistmatched license terms: ");
						final List<ILicenseTerm> mistmatched = new ArrayList<>(e.getMismatched());
						final int size = mistmatched.size();
						for (int i = 0; i < size; i++) {
							if (i > 0) c.append((i == size - 1) ? " & " : ", ");
							c.render(mistmatched.get(i), ILicenseTerm.class);
						}
					}
				}
			});
			builder.add(MultiLicenseFinding.class, e -> c -> appendLevel(e, c).append("Multiple, conflicting licenses detected for artifact"));
			builder.add(UnknownWorkTypeFinding.class, e -> c -> {
				appendLevel(e, c).append("Unknown work type");
				if (c.getMode().compareTo(ExplanationMode.Describe) >= 0) try (final ICloseable indent = c.indent()) {
					c.newline().append((c.getMode().compareTo(ExplanationMode.Trace) >= 0) ? HError.toString(e.getThrowable()) : e.getThrowable().getMessage());
				}
			});
			builder.add(UnrecognizedTermFinding.class, e -> c -> {
				final TermType termType = TermType.valueOf(e.getTerm());
				appendLevel(e, c).render(e.getTerm(), ITerm.class).append(" is not recognized so we cannot analyze ");
				switch (termType) {
					case License:
						c.append("whether the condition has been met.");
						break;
					case Usage:
						c.append("whether the licenses allow this usage.");
						break;
					default:
						throw new EnumException(TermType.class, termType);
				}
			});
			builder.add(ConditionFinding.class, e -> c -> {
				final IExpressionContext findingContext = c.getFindingContext();
				final Collection<ITerm> outputs = findingContext == null ? HCollection.emptyList() : findingContext.getOutputs();

				appendLevel(e, c).append("Condition");
				if (outputs.size() > 1) c.append('s');
				c.append(' ');
				if (!outputs.isEmpty()) {
					final List<ITerm> list = new ArrayList<>(outputs);
					final int size = list.size();
					for (int i = 0; i < size; i++) {
						if (i > 0) c.append((i == size - 1) ? " & " : ", ");
						c.render(list.get(i), ITerm.class);
					}
					c.append(' ');
				}

				c.append(outputs.size() > 1 ? "are" : "is");
				if (!e.isSatisfied()) c.append(" not");
				c.append(" satisfied");

				if (findingContext != null) {
					c.append(" based on ");
					final List<ITerm> list = new ArrayList<>(findingContext.getInputs());
					final int size = list.size();
					for (int i = 0; i < size; i++) {
						if (i > 0) c.append((i == size - 1) ? " & " : ", ");
						c.render(list.get(i), ITerm.class);
					}
				}

				explain(e, c);
			});
			builder.add(DiscloseSourceFinding.class, e -> c -> explain(e, appendLevel(e, c).append("You must disclose the source for this artifact")));
			builder.add(NoticeFinding.class, e -> c -> explain(e, appendLevel(e, c).append("You must publish a copyright and license notice stating that you use this artifact")));
			builder.add(StateChangesFinding.class, e -> c -> explain(e, appendLevel(e, c).append("You must state the changes you have made to your copy of this artifact")));
			builder.add(SuspiciousUsageFinding.class, e -> c -> explain(e, appendLevel(e, c).append("The usage for this artifact improperly specifies the ").append(e.getAttribute())));
		}
	}

	@Getter(lazy = true, value = AccessLevel.PROTECTED)
	private static final IRendering<Object, IReportRenderContext, IExplicitRenderable<? super IReportRenderContext>> renderingStatic = new ReportRendering();

	protected final ExplanationMode mode;

	protected final IConsumer2<TextNestedModified.TextNestedModifiedBuilder, Object> valueContext;

	public ReportRenderer() {
		this(ExplanationMode.Explain);
	}

	public ReportRenderer(ExplanationMode mode) {
		this(mode, (builder, value) -> builder.expression(value));
	}

	@Override
	protected ReportRenderContext createContext(TextNestedModified.TextNestedModifiedBuilder builder) {
		return new ReportRenderContext(builder, getMode(), getValueContext());
	}

	@Override
	protected IRendering<? super Object, ? extends IReportRenderContext, ? extends IExplicitRenderable<? super IReportRenderContext>> getRendering() {
		return getRenderingStatic();
	}
}
