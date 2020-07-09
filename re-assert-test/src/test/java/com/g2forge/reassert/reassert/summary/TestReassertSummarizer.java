package com.g2forge.reassert.reassert.summary;

import org.junit.Test;
import org.slf4j.event.Level;

import com.g2forge.alexandria.java.core.resource.Resource;
import com.g2forge.alexandria.java.io.dataaccess.ByteArrayDataSink;
import com.g2forge.alexandria.test.HAssert;
import com.g2forge.reassert.mock.MockCoordinates;
import com.g2forge.reassert.reassert.ReassertContext;
import com.g2forge.reassert.reassert.summary.ReassertSummarizer;
import com.g2forge.reassert.reassert.summary.model.ArtifactSummary;
import com.g2forge.reassert.reassert.summary.model.ReportSummary;
import com.g2forge.reassert.reassert.summary.model.ReportSummary.ReportSummaryBuilder;

public class TestReassertSummarizer {
	@Test
	public void artifacts() {
		final ReportSummaryBuilder summary = ReportSummary.builder();
		summary.artifact(ArtifactSummary.builder().level(Level.WARN).artifact(new MockCoordinates("A")).finding(null).build());
		summary.artifact(ArtifactSummary.builder().level(Level.ERROR).artifact(new MockCoordinates("B")).finding(null).build());

		final ByteArrayDataSink sink = new ByteArrayDataSink();
		new ReassertSummarizer(ReassertContext.getContext()).renderArtifacts(summary.build(), sink);
		HAssert.assertEquals(new Resource(getClass(), "artifacts.csv").read(false), sink.getStream().toString());
	}
}
