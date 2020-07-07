<xsl:stylesheet version="1.0"
	xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
	<xsl:output omit-xml-declaration="no" />

	<xsl:template match="node()|@*">
		<xsl:copy>
			<xsl:apply-templates select="node()|@*" />
		</xsl:copy>
	</xsl:template>

	<xsl:template match="/project/build/plugins/plugin">
		<xsl:copy>
			<version>2.3.1</version> <!-- Determined based on release dates for the two artifacts -->
			<xsl:call-template name="copy-children" />
		</xsl:copy>
	</xsl:template>

	<xsl:template name="copy-children">
		<xsl:copy-of select="./*" />
	</xsl:template>
</xsl:stylesheet>