package com.g2forge.reassert.license;

import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.jgrapht.Graph;

import com.g2forge.alexandria.annotations.note.Note;
import com.g2forge.alexandria.annotations.note.NoteType;
import com.g2forge.alexandria.java.core.enums.EnumException;
import com.g2forge.alexandria.java.core.helpers.HCollection;
import com.g2forge.alexandria.java.core.helpers.HCollector;
import com.g2forge.alexandria.java.core.marker.ISingleton;
import com.g2forge.alexandria.java.function.IFunction1;
import com.g2forge.alexandria.java.type.function.TypeSwitch1;
import com.g2forge.alexandria.java.type.ref.ATypeRef;
import com.g2forge.alexandria.java.type.ref.ITypeRef;
import com.g2forge.reassert.core.api.ReassertLegalOpinion;
import com.g2forge.reassert.core.model.Copy;
import com.g2forge.reassert.core.model.HReassertModel;
import com.g2forge.reassert.core.model.IEdge;
import com.g2forge.reassert.core.model.IVertex;
import com.g2forge.reassert.core.model.artifact.Artifact;
import com.g2forge.reassert.core.model.artifact.Depends;
import com.g2forge.reassert.core.model.artifact.Inherits;
import com.g2forge.reassert.core.model.contract.ITerms;
import com.g2forge.reassert.core.model.contract.Notice;
import com.g2forge.reassert.core.model.contract.license.ILicense;
import com.g2forge.reassert.core.model.contract.license.ILicenseTerm;
import com.g2forge.reassert.core.model.report.IReport;
import com.g2forge.reassert.core.model.report.Report;
import com.g2forge.reassert.core.model.work.IWorkType;
import com.g2forge.reassert.core.model.work.IWorkTypeFactory;
import com.g2forge.reassert.core.model.work.IncompatibleWorkLicenseFinding;
import com.g2forge.reassert.core.model.work.Work;
import com.g2forge.reassert.core.model.work.WorkContains;
import com.g2forge.reassert.core.model.work.WorkLicense;
import com.g2forge.reassert.term.StandardLicenseTerm;

import lombok.AccessLevel;
import lombok.Getter;

@ReassertLegalOpinion
public class StandardWorkTypeFactory implements IWorkTypeFactory, ISingleton {
	protected static class GPLWorkType implements IWorkType {
		@Getter(lazy = true, value = AccessLevel.PROTECTED)
		private final IFunction1<IEdge, Boolean> function = computeFunction();

		protected IFunction1<IEdge, Boolean> computeFunction() {
			final TypeSwitch1.FunctionBuilder<IEdge, Boolean> builder = new TypeSwitch1.FunctionBuilder<>();
			builder.add(Depends.class, e -> true);
			builder.add(Copy.class, e -> true);
			builder.add(Inherits.class, e -> true);
			return builder.build();
		}

		@Note(type = NoteType.TODO, value = "Use EEE logic package to explain our reasoning better (not really necessary, but it'll help)")
		protected IncompatibleWorkLicenseFinding isCompatible(ILicense license, ILicense test) {
			if ((license == test) || license.equals(test)) return null;

			final ITerms<ILicenseTerm> licenseTerms = license.getTerms(), testTerms = test.getTerms();
			final IncompatibleWorkLicenseFinding.IncompatibleWorkLicenseFindingBuilder builder = IncompatibleWorkLicenseFinding.builder();

			final Collection<ILicenseTerm> unknown;
			if (!licenseTerms.getSpecifiedTerms().equals(testTerms.getSpecifiedTerms())) {
				final Set<ILicenseTerm> union = HCollection.union(licenseTerms.getSpecifiedTerms(), testTerms.getSpecifiedTerms());
				final Set<ILicenseTerm> intersection = HCollection.intersection(licenseTerms.getSpecifiedTerms(), testTerms.getSpecifiedTerms());
				unknown = HCollection.difference(union, intersection);
				builder.unknown(unknown);
			} else unknown = HCollection.emptySet();
			builder.unknown(unknown);

			for (ILicenseTerm term : licenseTerms.getSpecifiedTerms()) {
				// Skip any terms that we already have a problem with, and oddly the GPL doesn't require you grant patents to other software for compatibility (example: https://www.gnu.org/licenses/license-list.en.html#ModifiedBSD)
				if (unknown.contains(term) || StandardLicenseTerm.PatentGrant.equals(term)) continue;
				switch (term.getType()) {
					case Permission:
						if (licenseTerms.isIncluded(term) && !testTerms.isIncluded(term)) builder.mismatched(term);
						break;
					case Condition:
					case Limitation:
						if (!licenseTerms.isIncluded(term) && testTerms.isIncluded(term)) builder.mismatched(term);
						break;
					default:
						throw new EnumException(ILicenseTerm.Type.class, term.getType());
				}
			}
			return builder.build();
		}

		@Override
		public boolean isIncluded(IEdge edge, boolean outgoing) {
			return getFunction().apply(edge);
		}

		@Override
		public IReport report(Graph<IVertex, IEdge> graph, Work work) {
			final Collection<Artifact<?>> artifacts = HReassertModel.get(graph, work, true, WorkContains.class::isInstance, new ATypeRef<Artifact<?>>() {});
			final Map<ILicense, List<Artifact<?>>> licenses = artifacts.stream().collect(HCollector.multiGroupingBy(artifact -> HReassertModel.get(graph, artifact, true, Notice.class::isInstance, ITypeRef.of(ILicense.class))));

			final ILicense license = HCollection.getOne(HReassertModel.get(graph, work, true, WorkLicense.class::isInstance, ITypeRef.of(ILicense.class)));
			final Report.ReportBuilder report = Report.builder();
			for (Map.Entry<ILicense, List<Artifact<?>>> entry : licenses.entrySet()) {
				final IncompatibleWorkLicenseFinding finding = isCompatible(license, entry.getKey());
				if (finding != null) report.finding(finding);
			}
			return report.build();
		}
	}

	protected static final StandardWorkTypeFactory INSTANCE = new StandardWorkTypeFactory();

	public static StandardWorkTypeFactory create() {
		return INSTANCE;
	}

	@Getter(lazy = true, value = AccessLevel.PROTECTED)
	private final IFunction1<ILicense, IWorkType> function = computeFunction();

	protected StandardWorkTypeFactory() {}

	protected IFunction1<ILicense, IWorkType> computeFunction() {
		final TypeSwitch1.FunctionBuilder<ILicense, IWorkType> builder = new TypeSwitch1.FunctionBuilder<>();
		builder.add(StandardLicense.class, l -> {
			switch (l) {
				case Owner:
				case BSD3:
				case Apache2:
					return null;
				case GPL3OrLater:
					return new GPLWorkType();
				default:
					throw new EnumException(StandardLicense.class, l);
			}
		});
		return builder.build();
	}

	@Override
	public IWorkType computeWorkType(ILicense license) {
		return getFunction().apply(license);
	}
}
